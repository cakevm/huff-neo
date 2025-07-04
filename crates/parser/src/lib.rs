#![doc = include_str!("../README.md")]
#![warn(missing_docs)]
#![warn(unused_extern_crates)]
#![forbid(unsafe_code)]

use alloy_primitives::{hex, keccak256};
use huff_neo_utils::ast::abi::{Argument, ArgumentLocation, EventDefinition, FunctionDefinition, FunctionType};
use huff_neo_utils::ast::huff::*;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::bytecode::Bytes;
use huff_neo_utils::file::remapper;
use huff_neo_utils::{
    error::*,
    prelude::{Span, bytes32_to_hex_string, str_to_bytes32},
    token::{Token, TokenKind},
    types::*,
};

/// The Parser
#[derive(Debug, Clone)]
pub struct Parser {
    /// Vector of the tokens
    pub tokens: Vec<Token>,
    /// Current position
    pub cursor: usize,
    /// Current token
    pub current_token: Token,
    /// Current base path for resolving imports
    pub base: Option<String>,
    /// A collection of current spans
    pub spans: Vec<Span>,
    /// Our remapper
    pub remapper: remapper::Remapper,
}

impl Parser {
    /// Public associated function that instantiates a Parser.
    pub fn new(tokens: Vec<Token>, base: Option<String>) -> Self {
        let initial_token = tokens.first().unwrap().clone();
        let remapper = remapper::Remapper::new("./");
        Self { tokens, cursor: 0, current_token: initial_token, base, spans: vec![], remapper }
    }

    /// Resets the current token and cursor to the first token in the parser's token vec
    ///
    /// PANICS if the tokens vec is empty!
    pub fn reset(&mut self) {
        self.current_token = self.tokens.first().unwrap().clone();
        self.cursor = 0;
    }

    /// Parse
    pub fn parse(&mut self) -> Result<Contract, ParserError> {
        // Remove all whitespaces, newlines, and comments first
        self.tokens.retain(|token| !matches!(token.kind, TokenKind::Whitespace | TokenKind::Comment(_)));

        // Reset the initial token
        self.reset();

        // Initialize an empty Contract
        let mut contract = Contract::default();

        // Iterate over tokens and construct the Contract aka AST
        while !self.check(TokenKind::Eof) {
            // Reset our spans
            self.spans = vec![];

            // Check for imports with the "#include" keyword
            if self.check(TokenKind::Include) {
                contract.imports.push(self.parse_imports()?);
            }
            // Check for a decorator above a test macro
            else if self.check(TokenKind::Pound) {
                let m = self.parse_macro(&mut contract)?;
                tracing::info!(target: "parser", "SUCCESSFULLY PARSED MACRO {}", m.name);
                contract.macros.insert(m.name.clone(), m);
            }
            // Check for a definition with the "#define" keyword
            else if self.check(TokenKind::Define) {
                // Consume the definition token
                self.match_kind(TokenKind::Define)?;

                // match to function, constant, macro, or event
                match self.current_token.kind {
                    TokenKind::Function => {
                        let func = self.parse_function()?;
                        tracing::info!(target: "parser", "SUCCESSFULLY PARSED FUNCTION {}", func.name);
                        contract.functions.push(func);
                    }
                    TokenKind::Event => {
                        let ev = self.parse_event()?;
                        tracing::info!(target: "parser", "SUCCESSFULLY PARSED EVENT {}", ev.name);
                        contract.events.push(ev);
                    }
                    TokenKind::Constant => {
                        let c = self.parse_constant()?;
                        tracing::info!(target: "parser", "SUCCESSFULLY PARSED CONSTANT {}", c.name);
                        contract.constants.lock().unwrap().push(c);
                    }
                    TokenKind::Error => {
                        let e = self.parse_custom_error()?;
                        tracing::info!(target: "parser", "SUCCESSFULLY PARSED ERROR {}", e.name);
                        contract.errors.push(e);
                    }
                    TokenKind::Macro | TokenKind::Fn => {
                        let m = self.parse_macro(&mut contract)?;
                        tracing::info!(target: "parser", "SUCCESSFULLY PARSED MACRO {}", m.name);
                        self.check_duplicate_macro(&contract, &m)?;
                        contract.macros.insert(m.name.clone(), m);
                    }
                    TokenKind::Test => {
                        let m = self.parse_macro(&mut contract)?;
                        tracing::info!(target: "parser", "SUCCESSFULLY PARSED TEST {}", m.name);
                        contract.macros.insert(m.name.clone(), m);
                    }
                    TokenKind::JumpTable | TokenKind::JumpTablePacked | TokenKind::CodeTable => {
                        contract.tables.push(self.parse_table()?);
                    }
                    _ => {
                        tracing::error!(
                            target: "parser",
                            "Invalid definition. Must be a function, event, constant, error, or macro. Got: {}",
                            self.current_token.kind
                        );
                        return Err(ParserError {
                            kind: ParserErrorKind::InvalidDefinition(self.current_token.kind.clone()),
                            hint: Some(
                                "Definition must be one of: `function`, `event`, `constant`, `error`, `macro`, `fn`, or `test`."
                                    .to_string(),
                            ),
                            spans: AstSpan(vec![self.current_token.span.clone()]),
                            cursor: self.cursor,
                        });
                    }
                };
            } else {
                // If we don't have an "#include" or "#define" keyword, we have an invalid token
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedType(self.current_token.kind.clone()),
                    hint: Some(format!("Expected either \"{}\" or \"{}\"", TokenKind::Define, TokenKind::Include)),
                    spans: AstSpan(vec![self.current_token.span.clone()]),
                    cursor: self.cursor,
                });
            }
        }

        Ok(contract)
    }

    /// Parses Contract Imports
    pub fn parse_imports(&mut self) -> Result<FilePath, ParserError> {
        // First token should be keyword "#include"
        self.match_kind(TokenKind::Include)?;

        // Then let's grab and validate the file path
        self.match_kind(TokenKind::Str("x".to_string()))?;
        let tok = self.peek_behind().unwrap().kind;
        let p = match tok {
            TokenKind::Str(file_path) => file_path,
            _ => {
                tracing::error!(target: "parser", "INVALID IMPORT PATH: {}", tok);
                let new_spans = self.spans.clone();
                self.spans = vec![];
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidName(tok.clone()),
                    hint: Some(format!("Expected import string. Got: \"{tok}\"")),
                    spans: AstSpan(new_spans),
                    cursor: self.cursor,
                });
            }
        };

        Ok(std::path::PathBuf::from(p))
    }

    /// Match current token to a type.
    pub fn match_kind(&mut self, kind: TokenKind) -> Result<TokenKind, ParserError> {
        if std::mem::discriminant(&self.current_token.kind) == std::mem::discriminant(&kind) {
            let curr_kind: TokenKind = self.current_token.kind.clone();
            self.consume();
            Ok(curr_kind)
        } else {
            tracing::error!(target: "parser", "TOKEN MISMATCH - EXPECTED: {}, GOT: {}", kind, self.current_token.kind);
            Err(ParserError {
                kind: ParserErrorKind::UnexpectedType(self.current_token.kind.clone()),
                hint: Some(format!("Expected: \"{kind}\"")),
                spans: AstSpan(self.spans.clone()),
                cursor: self.cursor,
            })
        }
    }

    /// Check the current token's type against the given type.
    pub fn check(&mut self, kind: TokenKind) -> bool {
        std::mem::discriminant(&self.current_token.kind) == std::mem::discriminant(&kind)
    }

    /// Checks whether the input label is unique in a macro.
    fn check_duplicate_label(&self, contract: &mut Contract, macro_name: &str, label: String, span: Span) -> Result<(), ParserError> {
        let key = format!("{}{macro_name}{label}", span.clone().file.map(|f| f.path.clone()).unwrap_or_default());

        if contract.labels.contains(&key) {
            tracing::error!(target: "parser", "DUPLICATED LABEL NAME: {}", label);
            Err(ParserError {
                kind: ParserErrorKind::DuplicateLabel(label.clone()),
                hint: Some(format!("Duplicated label name: \"{label}\" in macro: \"{macro_name}\"")),
                spans: AstSpan(vec![span]),
                cursor: self.cursor,
            })
        } else {
            contract.labels.insert(key);
            Ok(())
        }
    }

    /// Checks if there is a duplicate macro name
    pub fn check_duplicate_macro(&self, contract: &Contract, m: &MacroDefinition) -> Result<(), ParserError> {
        if contract.macros.contains_key(&m.name) {
            tracing::error!(target: "parser", "DUPLICATE MACRO NAME FOUND: {}",  m.name);
            Err(ParserError {
                kind: ParserErrorKind::DuplicateMacro(m.name.to_owned()),
                hint: Some("MACRO names should be unique".to_string()),
                spans: AstSpan(vec![m.span[2].clone()]),
                cursor: self.cursor,
            })
        } else {
            Ok(())
        }
    }

    /// Consumes the next token.
    pub fn consume(&mut self) {
        self.spans.push(self.current_token.span.clone());
        self.current_token = self.peek().unwrap();
        self.cursor += 1;
    }

    /// Consumes following tokens until not contained in the kinds vec of TokenKinds.
    pub fn consume_all(&mut self, kinds: Vec<TokenKind>) {
        loop {
            let token = self.peek().unwrap();
            if !kinds.contains(&token.kind) {
                break;
            }
            self.current_token = token;
            self.cursor += 1;
        }
    }

    /// Take a look at next token without consuming.
    pub fn peek(&mut self) -> Option<Token> {
        if self.cursor >= self.tokens.len() { None } else { Some(self.tokens.get(self.cursor + 1).unwrap().clone()) }
    }

    /// Take a look at the previous token.
    pub fn peek_behind(&self) -> Option<Token> {
        if self.cursor == 0 || self.cursor > self.tokens.len() { None } else { Some(self.tokens.get(self.cursor - 1).unwrap().clone()) }
    }

    /// Parses a function.
    /// Adheres to <https://github.com/huff-language/huffc/blob/master/src/parser/high-level.ts#L87-L111>
    pub fn parse_function(&mut self) -> Result<FunctionDefinition, ParserError> {
        // the first token should be of `TokenKind::Function`
        self.match_kind(TokenKind::Function)?;
        // function name should be next
        self.match_kind(TokenKind::Ident("x".to_string()))?;
        let tok = self.peek_behind().unwrap().kind;
        let name = match tok {
            TokenKind::Ident(fn_name) => fn_name,
            _ => {
                tracing::error!(target: "parser", "TOKEN MISMATCH - EXPECTED IDENT, GOT: {}", tok);
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidName(tok.clone()),
                    hint: Some(format!("Expected function name, found: \"{tok}\"")),
                    spans: AstSpan(self.spans.clone()),
                    cursor: self.cursor,
                });
            }
        };

        // function inputs should be next
        let inputs = self.parse_args(true, true, false)?;
        // function type should be next
        let fn_type = match self.current_token.kind.clone() {
            TokenKind::View => FunctionType::View,
            TokenKind::Pure => FunctionType::Pure,
            TokenKind::Payable => FunctionType::Payable,
            TokenKind::NonPayable => FunctionType::NonPayable,
            tok => {
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedType(tok),
                    hint: Some("Expected one of: `view`, `pure`, `payable`, `nonpayable`.".to_string()),
                    spans: AstSpan(vec![self.current_token.span.clone()]),
                    cursor: self.cursor,
                });
            }
        };
        // consume the function type
        self.consume();

        // next token should be of `TokenKind::Returns`
        self.match_kind(TokenKind::Returns)?;
        // function outputs should be next
        let outputs = self.parse_args(true, true, false)?;

        let input_types = inputs.iter().map(|i| i.arg_type.as_ref().unwrap().clone()).collect::<Vec<_>>();
        let method_signature = format!("{name}({})", input_types.join(","));
        let selector = keccak256(method_signature)[..4].try_into().unwrap();

        Ok(FunctionDefinition { name, signature: selector, inputs, fn_type, outputs, span: AstSpan(self.spans.clone()) })
    }

    /// Parse an event.
    pub fn parse_event(&mut self) -> Result<EventDefinition, ParserError> {
        // The event should start with `TokenKind::Event`
        self.match_kind(TokenKind::Event)?;

        // Parse the event name
        self.match_kind(TokenKind::Ident("x".to_string()))?;
        let tok = self.peek_behind().unwrap().kind;

        let name = match tok {
            TokenKind::Ident(event_name) => event_name,
            _ => {
                tracing::error!(target: "parser", "TOKEN MISMATCH - EXPECTED IDENT, GOT: {}", tok);
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidName(tok.clone()),
                    hint: Some(format!("Expected event name, found: \"{tok}\"")),
                    spans: AstSpan(self.spans.clone()),
                    cursor: self.cursor,
                });
            }
        };

        // Parse the event's parameters
        let parameters = self.parse_args(true, true, true)?;

        let input_types = parameters.iter().map(|i| i.arg_type.as_ref().unwrap().clone()).collect::<Vec<_>>();
        let event_signature = format!("{name}({})", input_types.join(","));
        let event_selector = keccak256(event_signature).0;

        Ok(EventDefinition { name, parameters, span: AstSpan(self.spans.clone()), hash: event_selector })
    }

    /// Parse a constant.
    pub fn parse_constant(&mut self) -> Result<ConstantDefinition, ParserError> {
        // Constant Identifier
        self.match_kind(TokenKind::Constant)?;

        // Parse the constant name
        self.match_kind(TokenKind::Ident("x".to_string()))?;
        let tok = self.peek_behind().unwrap().kind;
        let name = match tok {
            TokenKind::Ident(const_name) => const_name,
            _ => {
                tracing::error!(target: "parser", "TOKEN MISMATCH - EXPECTED IDENT, GOT: {}", tok);
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedType(tok),
                    hint: Some("Expected constant name.".to_string()),
                    spans: AstSpan(self.spans.clone()),
                    cursor: self.cursor,
                });
            }
        };

        // We must assign a value to the constant
        self.match_kind(TokenKind::Assign)?;

        let value: ConstVal = match self.current_token.kind.clone() {
            TokenKind::FreeStoragePointer => {
                self.consume();
                ConstVal::FreeStoragePointer(FreeStoragePointer {})
            }
            TokenKind::Bytes(l) => {
                self.consume();
                ConstVal::Bytes(Bytes(l))
            }
            TokenKind::BuiltinFunction(f) => {
                let curr_spans = vec![self.current_token.span.clone()];
                self.match_kind(TokenKind::BuiltinFunction(String::default()))?;
                let args = self.parse_builtin_args()?;
                ConstVal::BuiltinFunctionCall(BuiltinFunctionCall { kind: BuiltinFunctionKind::from(f), args, span: AstSpan(curr_spans) })
            }
            kind => {
                tracing::error!(target: "parser", "TOKEN MISMATCH - EXPECTED FreeStoragePointer OR Hex, GOT: {}", self.current_token.kind);
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidConstantValue(kind),
                    hint: Some("Expected constant value to be Hex or `FREE_STORAGE_POINTER()`".to_string()),
                    spans: AstSpan(vec![self.current_token.span.clone()]),
                    cursor: self.cursor,
                });
            }
        };

        // Clone spans and set to nothing
        let new_spans = self.spans.clone();
        self.spans = vec![];

        // Return the Constant Definition
        Ok(ConstantDefinition { name, value, span: AstSpan(new_spans) })
    }

    /// Parse a custom error definition.
    pub fn parse_custom_error(&mut self) -> Result<ErrorDefinition, ParserError> {
        // Error Identifier
        self.match_kind(TokenKind::Error)?;

        // Parse the error name
        self.match_kind(TokenKind::Ident("x".to_string()))?;
        let tok = self.peek_behind().unwrap().kind;
        let name = match tok {
            TokenKind::Ident(err_name) => err_name,
            _ => {
                tracing::error!(target: "parser", "TOKEN MISMATCH - EXPECTED IDENT, GOT: {}", tok);
                return Err(ParserError {
                    kind: ParserErrorKind::UnexpectedType(tok),
                    hint: Some("Expected error name.".to_string()),
                    spans: AstSpan(self.spans.clone()),
                    cursor: self.cursor,
                });
            }
        };

        // Get arguments for signature
        let parameters = self.parse_args(true, true, false)?;

        let input_types = parameters.iter().map(|i| i.arg_type.as_ref().unwrap().clone()).collect::<Vec<_>>();
        let method_signature = format!("{name}({})", input_types.join(","));
        let selector = keccak256(method_signature)[..4].try_into().unwrap();

        // Clone spans and set to nothing
        let new_spans = self.spans.clone();
        self.spans = vec![];

        Ok(ErrorDefinition { name, selector, parameters, span: AstSpan(new_spans) })
    }

    /// Parses a decorator.
    ///
    /// Decorators are currently used to add additional flags to a test.
    pub fn parse_decorator(&mut self) -> Result<Decorator, ParserError> {
        self.match_kind(TokenKind::Pound)?;
        self.match_kind(TokenKind::OpenBracket)?;

        let mut flags: Vec<DecoratorFlag> = Vec::default();

        while !self.check(TokenKind::CloseBracket) {
            if let TokenKind::Ident(s) = self.match_kind(TokenKind::Ident(String::default()))? {
                // Consume the open parenthesis
                self.consume();

                match DecoratorFlag::try_from(&s) {
                    // The calldata flag accepts a single string as an argument
                    Ok(DecoratorFlag::Calldata(_)) => {
                        if let TokenKind::Str(s) = &self.match_kind(TokenKind::Str(String::default()))? {
                            flags.push(DecoratorFlag::Calldata(s.clone()));
                        } else {
                            return Err(ParserError {
                                kind: ParserErrorKind::InvalidDecoratorFlagArg(self.current_token.kind.clone()),
                                hint: Some(format!("Expected string for decorator flag: {s}")),
                                spans: AstSpan(vec![self.current_token.span.clone()]),
                                cursor: self.cursor,
                            });
                        }
                    }
                    // The value flag accepts a single literal as an argument
                    Ok(DecoratorFlag::Value(_)) => {
                        if let TokenKind::Literal(l) = self.match_kind(TokenKind::Literal(Literal::default()))? {
                            flags.push(DecoratorFlag::Value(l));
                        } else {
                            return Err(ParserError {
                                kind: ParserErrorKind::InvalidDecoratorFlagArg(self.current_token.kind.clone()),
                                hint: Some(format!("Expected literal for decorator flag: {s}")),
                                spans: AstSpan(vec![self.current_token.span.clone()]),
                                cursor: self.cursor,
                            });
                        }
                    }
                    Err(_) => {
                        tracing::error!(target: "parser", "DECORATOR FLAG NOT FOUND: {}", s);
                        return Err(ParserError {
                            kind: ParserErrorKind::InvalidDecoratorFlag(s.clone()),
                            hint: Some(format!("Unknown decorator flag: {s}")),
                            spans: AstSpan(self.spans.clone()),
                            cursor: self.cursor,
                        });
                    }
                }

                // Consume the closing parenthesis
                self.match_kind(TokenKind::CloseParen)?;

                // Multiple flags are possible
                if self.check(TokenKind::Comma) {
                    self.consume();
                }
            } else {
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidDecoratorFlag(String::from("EXPECTED DECORATOR FLAG")),
                    hint: Some(String::from("Unknown decorator flag")),
                    spans: AstSpan(self.spans.clone()),
                    cursor: self.cursor,
                });
            }
        }

        // Consume the final bracket
        self.consume();

        Ok(Decorator { flags })
    }

    /// Parses a macro.
    ///
    /// It should parse the following : macro MACRO_NAME(args...) = takes (x) returns (n) {...}
    pub fn parse_macro(&mut self, contract: &mut Contract) -> Result<MacroDefinition, ParserError> {
        let mut decorator: Option<Decorator> = None;
        if self.check(TokenKind::Pound) {
            decorator = Some(self.parse_decorator()?);

            // Consume the `#define` keyword
            self.consume();
        }

        let outlined = self.check(TokenKind::Fn);
        let test = self.check(TokenKind::Test);

        self.match_kind(if outlined {
            TokenKind::Fn
        } else if test {
            TokenKind::Test
        } else {
            TokenKind::Macro
        })?;

        let macro_name: String = self.match_kind(TokenKind::Ident("MACRO_NAME".to_string()))?.to_string();
        tracing::info!(target: "parser", "PARSING MACRO: \"{}\"", macro_name);

        let macro_arguments = self.parse_args(true, false, false)?;
        self.match_kind(TokenKind::Assign)?;

        let macro_takes = self.match_kind(TokenKind::Takes).map_or(Ok(0), |_| self.parse_single_arg())?;
        let macro_returns = self.match_kind(TokenKind::Returns).map_or(Ok(0), |_| self.parse_single_arg())?;

        let macro_statements: Vec<Statement> = self.parse_body(&macro_name, contract)?;

        Ok(MacroDefinition::new(
            macro_name,
            decorator,
            macro_arguments,
            macro_statements,
            macro_takes,
            macro_returns,
            self.spans.clone(),
            outlined,
            test,
        ))
    }

    /// Parse the body of a macro.
    ///
    /// Only HEX, OPCODES, labels, builtins, and MACRO calls should be authorized.
    pub fn parse_body(&mut self, macro_name: &str, contract: &mut Contract) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = Vec::new();
        self.match_kind(TokenKind::OpenBrace)?;
        tracing::info!(target: "parser", "PARSING MACRO BODY");
        while !self.check(TokenKind::CloseBrace) {
            match self.current_token.kind.clone() {
                TokenKind::Literal(val) => {
                    let curr_spans = vec![self.current_token.span.clone()];
                    tracing::info!(target: "parser", "PARSING MACRO BODY: [LITERAL: {}]", hex::encode(val));
                    self.consume();
                    statements.push(Statement { ty: StatementType::Literal(val), span: AstSpan(curr_spans) });
                }
                TokenKind::Opcode(o) => {
                    let curr_spans = vec![self.current_token.span.clone()];
                    tracing::info!(target: "parser", "PARSING MACRO BODY: [OPCODE: {}]", o);
                    self.consume();
                    statements.push(Statement { ty: StatementType::Opcode(o), span: AstSpan(curr_spans) });
                    // If the opcode is a push that takes a literal value, we need to parse the next
                    // literal
                    if o.is_value_push() {
                        match self.current_token.kind {
                            TokenKind::Literal(val) => {
                                let curr_spans = vec![self.current_token.span.clone()];
                                tracing::info!(target: "parser", "PARSING MACRO BODY: [LITERAL: {}]", hex::encode(val));
                                self.consume();

                                // Check that the literal does not overflow the push size
                                let hex_literal: String = bytes32_to_hex_string(&val, false);
                                if o.push_overflows(&hex_literal) {
                                    return Err(ParserError {
                                        kind: ParserErrorKind::InvalidPush(o),
                                        hint: Some(format!("Literal {hex_literal:?} contains too many bytes for opcode \"{o:?}\"")),
                                        spans: AstSpan(curr_spans),
                                        cursor: self.cursor,
                                    });
                                }

                                // Otherwise we can push the literal
                                statements.push(Statement { ty: StatementType::Literal(val), span: AstSpan(curr_spans) });
                            }
                            _ => {
                                return Err(ParserError {
                                    kind: ParserErrorKind::InvalidPush(o),
                                    hint: Some(format!("Expected literal following \"{:?}\", found \"{:?}\"", o, self.current_token.kind)),
                                    spans: AstSpan(vec![self.current_token.span.clone()]),
                                    cursor: self.cursor,
                                });
                            }
                        }
                    }
                }
                TokenKind::Ident(ident_str) => {
                    let mut curr_spans = vec![self.current_token.span.clone()];
                    tracing::info!(target: "parser", "PARSING MACRO BODY: [IDENT: {}]", ident_str);
                    self.match_kind(TokenKind::Ident("MACRO_NAME".to_string()))?;
                    // Can be a macro call or label call
                    match self.current_token.kind {
                        TokenKind::OpenParen => {
                            // Parse Macro Call
                            let lit_args = self.parse_macro_call_args(macro_name.to_owned())?;
                            // Grab all spans following our macro invocation spam
                            if let Some(i) = self.spans.iter().position(|s| s.eq(&curr_spans[0])) {
                                curr_spans.append(&mut self.spans[(i + 1)..].to_vec());
                            }
                            statements.push(Statement {
                                ty: StatementType::MacroInvocation(MacroInvocation {
                                    macro_name: ident_str.to_string(),
                                    args: lit_args,
                                    span: AstSpan(curr_spans.clone()),
                                }),
                                span: AstSpan(curr_spans),
                            });
                        }
                        _ => {
                            tracing::info!(target: "parser", "LABEL CALL TO: {}", ident_str);
                            statements.push(Statement { ty: StatementType::LabelCall(ident_str), span: AstSpan(curr_spans) });
                        }
                    }
                }
                TokenKind::Label(l) => {
                    let mut curr_spans = vec![self.current_token.span.clone()];
                    self.consume();
                    self.check_duplicate_label(contract, macro_name, l.to_string(), self.current_token.span.clone())?;
                    let inner_statements: Vec<Statement> = self.parse_label(macro_name.to_owned())?;
                    inner_statements.iter().for_each(|a| curr_spans.extend_from_slice(a.span.inner_ref()));
                    tracing::info!(target: "parser", "PARSED LABEL \"{}\" INSIDE MACRO WITH {} STATEMENTS.", l, inner_statements.len());
                    statements.push(Statement {
                        ty: StatementType::Label(Label { name: l, inner: inner_statements, span: AstSpan(curr_spans.clone()) }),
                        span: AstSpan(curr_spans),
                    });
                }
                TokenKind::OpenBracket => {
                    let (constant, const_span) = self.parse_constant_push()?;
                    tracing::info!(target: "parser", "PARSING MACRO BODY: [CONSTANT: {}]", constant);
                    statements.push(Statement { ty: StatementType::Constant(constant), span: AstSpan(vec![const_span]) });
                }
                TokenKind::LeftAngle => {
                    let (arg_call, arg_span) = self.parse_arg_call()?;
                    tracing::info!(target: "parser", "PARSING MACRO BODY: [ARG CALL: {}]", arg_call);
                    statements
                        .push(Statement { ty: StatementType::ArgCall(macro_name.to_owned(), arg_call), span: AstSpan(vec![arg_span]) });
                }
                TokenKind::BuiltinFunction(f) => {
                    let mut curr_spans = vec![self.current_token.span.clone()];
                    self.match_kind(TokenKind::BuiltinFunction(String::default()))?;
                    let args = self.parse_builtin_args()?;
                    args.iter().for_each(|a| curr_spans.extend_from_slice(a.span().inner_ref()));
                    tracing::info!(target: "parser", "PARSING MACRO BODY: [BUILTIN FN: {}({:?})]", f, args);
                    statements.push(Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::from(f),
                            args,
                            span: AstSpan(curr_spans.clone()),
                        }),
                        span: AstSpan(curr_spans),
                    });
                }
                kind => {
                    tracing::error!(target: "parser", "TOKEN MISMATCH - MACRO BODY: {}", kind);
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidTokenInMacroBody(kind),
                        hint: None,
                        spans: AstSpan(vec![self.current_token.span.clone()]),
                        cursor: self.cursor,
                    });
                }
            };
        }
        // consume close brace
        self.match_kind(TokenKind::CloseBrace)?;
        Ok(statements)
    }

    /// Parse the body of a label.
    ///
    /// ## Examples
    ///
    /// Below is an example of a label that contains a Macro Invocation, Literals, and Opcodes.
    ///
    /// ```huff
    /// error:
    ///     TRANSFER()
    ///     0x20 0x00 return
    /// ```
    pub fn parse_label(&mut self, parent_macro_name: String) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = Vec::new();
        self.match_kind(TokenKind::Colon)?;
        while !self.check(TokenKind::Label("NEXT_LABEL".to_string())) && !self.check(TokenKind::CloseBrace) {
            match self.current_token.kind.clone() {
                TokenKind::Literal(val) => {
                    let curr_spans = vec![self.current_token.span.clone()];
                    tracing::info!(target: "parser", "PARSING LABEL BODY: [LITERAL: {}]", hex::encode(val));
                    self.consume();
                    statements.push(Statement { ty: StatementType::Literal(val), span: AstSpan(curr_spans) });
                }
                TokenKind::Opcode(o) => {
                    let curr_spans = vec![self.current_token.span.clone()];
                    tracing::info!(target: "parser", "PARSING LABEL BODY: [OPCODE: {}]", o);
                    self.consume();
                    statements.push(Statement { ty: StatementType::Opcode(o), span: AstSpan(curr_spans) });
                }
                TokenKind::Ident(ident_str) => {
                    let mut curr_spans = vec![self.current_token.span.clone()];
                    tracing::info!(target: "parser", "PARSING LABEL BODY: [IDENT: {}]", ident_str);
                    self.match_kind(TokenKind::Ident("MACRO_NAME".to_string()))?;
                    // Can be a macro call or label call
                    match self.current_token.kind.clone() {
                        TokenKind::OpenParen => {
                            // Parse Macro Call
                            let lit_args = self.parse_macro_call_args(ident_str.to_string())?;
                            // Grab all spans following our macro invocation spam
                            if let Some(i) = self.spans.iter().position(|s| s.eq(&curr_spans[0])) {
                                curr_spans.append(&mut self.spans[(i + 1)..].to_vec());
                            }
                            statements.push(Statement {
                                ty: StatementType::MacroInvocation(MacroInvocation {
                                    macro_name: ident_str.to_string(),
                                    args: lit_args,
                                    span: AstSpan(curr_spans.clone()),
                                }),
                                span: AstSpan(curr_spans),
                            });
                        }
                        _ => {
                            tracing::info!(target: "parser", "LABEL CALL TO: {}", ident_str);
                            statements.push(Statement { ty: StatementType::LabelCall(ident_str), span: AstSpan(curr_spans) });
                        }
                    }
                }
                TokenKind::OpenBracket => {
                    let (constant, const_span) = self.parse_constant_push()?;
                    tracing::info!(target: "parser", "PARSING LABEL BODY: [CONSTANT: {}]", constant);
                    statements.push(Statement { ty: StatementType::Constant(constant), span: AstSpan(vec![const_span]) });
                }
                TokenKind::LeftAngle => {
                    let (arg_call, arg_span) = self.parse_arg_call()?;
                    tracing::info!(target: "parser", "PARSING LABEL BODY: [ARG CALL: {}]", arg_call);
                    statements
                        .push(Statement { ty: StatementType::ArgCall(parent_macro_name.clone(), arg_call), span: AstSpan(vec![arg_span]) });
                }
                TokenKind::BuiltinFunction(f) => {
                    let mut curr_spans = vec![self.current_token.span.clone()];
                    self.match_kind(TokenKind::BuiltinFunction(String::default()))?;
                    let args = self.parse_builtin_args()?;
                    args.iter().for_each(|a| curr_spans.extend_from_slice(a.span().inner_ref()));
                    tracing::info!(target: "parser", "PARSING LABEL BODY: [BUILTIN FN: {}({:?})]", f, args);
                    statements.push(Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::from(f),
                            args,
                            span: AstSpan(curr_spans.clone()),
                        }),
                        span: AstSpan(curr_spans),
                    });
                }
                kind => {
                    tracing::error!(target: "parser", "TOKEN MISMATCH - LABEL BODY: {}", kind);
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidTokenInLabelDefinition(kind),
                        hint: None,
                        spans: AstSpan(vec![self.current_token.span.clone()]),
                        cursor: self.cursor,
                    });
                }
            };
        }
        Ok(statements)
    }

    /// Parse new lines.
    ///
    /// No-return since newlines are non-essential.
    pub fn parse_newline(&mut self) -> Result<(), ParserError> {
        self.match_kind(TokenKind::Whitespace)?;
        while self.check(TokenKind::Whitespace) {
            self.consume();
        }
        Ok(())
    }

    /// Parse the argument of a builtin function call.
    pub fn parse_builtin_args(&mut self) -> Result<Vec<BuiltinFunctionArg>, ParserError> {
        let mut args: Vec<BuiltinFunctionArg> = Vec::new();
        self.match_kind(TokenKind::OpenParen)?;

        tracing::debug!(target: "parser", "PARSING ARGs: {:?}", self.current_token.kind);
        while !self.check(TokenKind::CloseParen) {
            let mut is_builtin = false;
            let mut builtin = "".to_string();

            match &self.current_token.kind {
                TokenKind::Str(s) => {
                    args.push(BuiltinFunctionArg::Argument(Argument {
                        name: Some(s.to_owned()), // Place the string in the "name" field
                        arg_type: None,
                        indexed: false,
                        span: AstSpan(vec![self.current_token.span.clone()]),
                        arg_location: None,
                    }));
                    self.consume();
                }
                TokenKind::Bytes(bytes) => {
                    args.push(BuiltinFunctionArg::Argument(Argument {
                        name: Some(bytes.to_owned()),
                        arg_type: None,
                        indexed: false,
                        span: AstSpan(vec![self.current_token.span.clone()]),
                        arg_location: None,
                    }));
                    self.consume();
                }
                TokenKind::Literal(l) => {
                    args.push(BuiltinFunctionArg::Argument(Argument {
                        // Place literal in the "name" field
                        name: Some(bytes32_to_hex_string(l, false)),
                        arg_location: None,
                        arg_type: None,
                        indexed: false,
                        span: AstSpan(vec![self.current_token.span.clone()]),
                    }));
                    self.consume();
                }
                TokenKind::Ident(ident) => {
                    args.push(BuiltinFunctionArg::Argument(Argument {
                        name: Some(ident.to_owned()),
                        arg_type: None,
                        indexed: false,
                        span: AstSpan(vec![self.current_token.span.clone()]),
                        arg_location: None,
                    }));
                    self.consume();
                }
                TokenKind::BuiltinFunction(builtin_ref) => {
                    is_builtin = true;
                    builtin = builtin_ref.clone();
                    self.consume();
                }
                TokenKind::OpenBracket => {
                    let (constant_name, span) = self.parse_constant_push()?;
                    args.push(BuiltinFunctionArg::Constant(constant_name, AstSpan(vec![span])));
                }
                _ => {
                    return Err(ParserError {
                        kind: ParserErrorKind::UnexpectedType(self.current_token.kind.clone()),
                        hint: Some("Expected string, literal, hex, identifier or build-in".to_string()),
                        spans: AstSpan(vec![self.current_token.span.clone()]),
                        cursor: self.cursor,
                    });
                }
            }

            if is_builtin {
                args.push(BuiltinFunctionArg::BuiltinFunctionCall(BuiltinFunctionCall {
                    kind: BuiltinFunctionKind::from(builtin.clone()),
                    args: self.parse_builtin_args()?, // Recursively parse the arguments
                    span: AstSpan(vec![self.current_token.span.clone()]),
                }));
            }

            if self.check(TokenKind::Comma) {
                self.consume();
            }
        }

        // consume close parenthesis
        self.match_kind(TokenKind::CloseParen)?;
        Ok(args)
    }

    /// Parse abi arguments
    ///
    /// Arguments can be typed or not. Between parenthesis.
    /// Works for both inputs and outputs.
    /// It should parse the following : (uint256 a, bool b, ...)
    pub fn parse_args(&mut self, select_name: bool, select_type: bool, has_indexed: bool) -> Result<Vec<Argument>, ParserError> {
        let mut args: Vec<Argument> = Vec::new();
        self.match_kind(TokenKind::OpenParen)?;
        let mut on_type = true;
        tracing::debug!(target: "parser", "PARSING ABI ARGs: {:?}", self.current_token.kind);
        while !self.check(TokenKind::CloseParen) {
            let mut arg = Argument::default();
            let mut arg_spans = vec![];

            // type comes first
            if select_type {
                arg_spans.push(self.current_token.span.clone());
                arg.arg_type = Some(self.parse_arg_type()?.to_string());
                // Check if the argument is indexed
                if has_indexed && self.check(TokenKind::Indexed) {
                    arg.indexed = true;
                    arg_spans.push(self.current_token.span.clone());
                    self.consume(); // consume "indexed" keyword
                }
                on_type = false;
            }

            // It can also be a data location
            match &self.current_token.kind {
                TokenKind::Calldata => {
                    arg.arg_location = Some(ArgumentLocation::Calldata);
                    arg_spans.push(self.current_token.span.clone());
                    self.consume();
                }
                TokenKind::Memory => {
                    arg.arg_location = Some(ArgumentLocation::Memory);
                    arg_spans.push(self.current_token.span.clone());
                    self.consume();
                }
                TokenKind::Storage => {
                    arg.arg_location = Some(ArgumentLocation::Storage);
                    arg_spans.push(self.current_token.span.clone());
                    self.consume();
                }
                _ => {}
            }

            // name comes second (is optional)
            if select_name
                && (self.check(TokenKind::Ident("x".to_string())) || self.check(TokenKind::PrimitiveType(PrimitiveEVMType::Address)))
            {
                // We need to check if the name is a keyword - not the type
                if !on_type {
                    // Check for reserved primitive type keyword use and throw an error if so
                    match self.current_token.kind.clone() {
                        TokenKind::Ident(arg_str) => {
                            if PrimitiveEVMType::try_from(&arg_str).is_ok() {
                                return Err(ParserError {
                                    kind: ParserErrorKind::InvalidTypeAsArgumentName(self.current_token.kind.clone()),
                                    hint: Some(format!("Argument names cannot be EVM types: {arg_str}")),
                                    spans: AstSpan(vec![self.current_token.span.clone()]),
                                    cursor: self.cursor,
                                });
                            }
                        }
                        TokenKind::PrimitiveType(ty) => {
                            return Err(ParserError {
                                kind: ParserErrorKind::InvalidTypeAsArgumentName(self.current_token.kind.clone()),
                                hint: Some(format!("Argument names cannot be EVM types: {ty}")),
                                spans: AstSpan(vec![self.current_token.span.clone()]),
                                cursor: self.cursor,
                            });
                        }
                        _ => { /* continue, valid string */ }
                    }
                }
                arg_spans.push(self.current_token.span.clone());
                arg.name = Some(self.match_kind(TokenKind::Ident("x".to_string()))?.to_string());
                on_type = !on_type;
            }

            // multiple args possible
            if self.check(TokenKind::Comma) {
                self.consume();
                on_type = true;
            }

            // If both arg type and arg name are none, we didn't consume anything
            if arg.arg_type.is_none() && arg.name.is_none() {
                tracing::error!(target: "parser", "INVALID ARGUMENT TOKEN: {:?}", self.current_token.kind);
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidArgs(self.current_token.kind.clone()),
                    hint: None,
                    spans: AstSpan(vec![self.current_token.span.clone()]),
                    cursor: self.cursor,
                });
            }

            arg.span = AstSpan(arg_spans);

            args.push(arg);
        }
        // consume close parenthesis
        self.match_kind(TokenKind::CloseParen)?;
        Ok(args)
    }

    /// Parses the following : (x)
    pub fn parse_single_arg(&mut self) -> Result<usize, ParserError> {
        self.match_kind(TokenKind::OpenParen)?;
        let single_arg_span = vec![self.current_token.span.clone()];
        let value: usize = match self.match_kind(TokenKind::Num(0)) {
            Ok(TokenKind::Num(value)) => value,
            _ => {
                return Err(ParserError {
                    kind: ParserErrorKind::InvalidSingleArg(self.current_token.kind.clone()),
                    hint: Some("Expected number representing stack item count.".to_string()),
                    spans: AstSpan(single_arg_span),
                    cursor: self.cursor,
                });
            }
        };
        self.match_kind(TokenKind::CloseParen)?;
        Ok(value)
    }

    /// Parse the arguments of a macro call.
    pub fn parse_macro_call_args(&mut self, macro_name: String) -> Result<Vec<MacroArg>, ParserError> {
        let mut args = vec![];
        self.match_kind(TokenKind::OpenParen)?;
        while !self.check(TokenKind::CloseParen) {
            // We can pass either directly hex values or labels (without the ":")
            match self.current_token.kind.clone() {
                TokenKind::Literal(lit) => {
                    args.push(MacroArg::Literal(lit));
                    self.consume();
                }
                TokenKind::Opcode(o) => {
                    args.push(MacroArg::Opcode(o));
                    self.consume();
                }
                TokenKind::Ident(ident) => {
                    if self.peek().unwrap().kind == TokenKind::OpenParen {
                        // It's a nested macro call
                        let mut curr_spans = vec![self.current_token.span.clone()];
                        self.match_kind(TokenKind::Ident("MACRO_NAME".to_string()))?;
                        // Parse Macro Call
                        let lit_args = self.parse_macro_call_args(macro_name.clone())?;
                        // Grab all spans following our macro invocation spam
                        if let Some(i) = self.spans.iter().position(|s| s.eq(&curr_spans[0])) {
                            curr_spans.append(&mut self.spans[(i + 1)..].to_vec());
                        }
                        args.push(MacroArg::MacroCall(MacroInvocation {
                            macro_name: ident,
                            args: lit_args,
                            span: AstSpan(curr_spans.clone()),
                        }));
                    } else {
                        args.push(MacroArg::Ident(ident));
                        self.consume();
                    }
                }
                TokenKind::Calldata => {
                    args.push(MacroArg::Ident("calldata".to_string()));
                    self.consume();
                }
                TokenKind::LeftAngle => {
                    // Passed into the Macro Call like:
                    // GET_SLOT_FROM_KEY(<mem_ptr>)  // [slot]
                    self.consume();
                    let arg_name = self.match_kind(TokenKind::Ident("ARG_CALL".to_string()))?.to_string();
                    args.push(MacroArg::ArgCall(ArgCall {
                        macro_name: macro_name.clone(),
                        name: arg_name,
                        span: AstSpan(vec![self.current_token.span.clone()]),
                    }));
                    self.match_kind(TokenKind::RightAngle)?;
                }
                arg => {
                    tracing::error!(
                        target: "parser",
                        "Invalid macro call arguments. Must be of kind Ident or Literal. Got: {}",
                        self.current_token.kind
                    );
                    let new_spans = self.spans.clone();
                    self.spans = vec![];
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidMacroArgs(arg),
                        hint: Some("Expected literal, identifier (string), or an argument call".to_string()),
                        spans: AstSpan(new_spans),
                        cursor: self.cursor,
                    });
                }
            }
            if self.check(TokenKind::Comma) {
                self.consume();
            }
        }
        // consume close parenthesis
        self.consume();
        Ok(args)
    }

    /// Parses a table (JumpTable, JumpTablePacked, or CodeTable).
    ///
    /// It should parse the following : (jumptable|jumptable__packed|table) NAME() {...}
    pub fn parse_table(&mut self) -> Result<TableDefinition, ParserError> {
        let is_code_table = self.current_token.kind == TokenKind::CodeTable;
        let kind = TableKind::from(self.match_kind(self.current_token.kind.clone())?);
        let table_name: String = self.match_kind(TokenKind::Ident("TABLE_NAME".to_string()))?.to_string();

        // Parenthesis and assignment are optional
        let _ = self.match_kind(TokenKind::OpenParen);
        let _ = self.match_kind(TokenKind::CloseParen);
        let _ = self.match_kind(TokenKind::Assign);

        // Parse the core table
        let table_statements: Vec<Statement> = self.parse_table_body(is_code_table)?;
        let size = match kind {
            TableKind::JumpTablePacked => Some(table_statements.len() * 0x02),
            TableKind::JumpTable => Some(table_statements.len() * 0x20),
            TableKind::CodeTable => {
                let mut table_size = 0;
                // If builtins or constants are used, we cannot determine the size of the table. This will happen during code generation.
                let mut unknown_size = false;
                for s in &table_statements {
                    if let StatementType::Code(c) = &s.ty {
                        table_size += c.len();
                    } else if let StatementType::BuiltinFunctionCall(_) = &s.ty {
                        unknown_size = true;
                    } else if let StatementType::Constant(_) = &s.ty {
                        unknown_size = true;
                    } else {
                        return Err(ParserError {
                            kind: ParserErrorKind::InvalidTableStatement(format!("{}", s.ty)),
                            hint: Some(format!("Invalid table statement. Must be valid hex bytecode. Got: {s:?}")),
                            spans: AstSpan(s.span.inner_ref().to_vec()),
                            cursor: self.cursor,
                        });
                    }
                }
                if unknown_size { None } else { Some(table_size / 2) }
            }
        };

        let table_size = size.map(|s| str_to_bytes32(format!("{s:02x}").as_str()));
        Ok(TableDefinition::new(table_name, kind, table_statements, table_size, AstSpan(self.spans.clone())))
    }

    /// Parse the body of a table.
    ///
    /// Only `LabelCall` and `Code` Statements should be authorized.
    pub fn parse_table_body(&mut self, is_code_table: bool) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = Vec::new();

        self.match_kind(TokenKind::OpenBrace)?;
        while !self.check(TokenKind::CloseBrace) {
            let new_spans = vec![self.current_token.span.clone()];
            match self.current_token.kind.clone() {
                TokenKind::Ident(ident_str) => {
                    if is_code_table {
                        tracing::error!("Invalid CodeTable Body Token: {:?}", self.current_token.kind);
                        return Err(ParserError {
                            kind: ParserErrorKind::InvalidTableBodyToken(self.current_token.kind.clone()),
                            hint: Some("Expected valid hex bytecode.".to_string()),
                            spans: AstSpan(new_spans),
                            cursor: self.cursor,
                        });
                    }
                    statements.push(Statement { ty: StatementType::LabelCall(ident_str.to_string()), span: AstSpan(new_spans) });
                    self.consume();
                }
                TokenKind::Bytes(bytes) => {
                    if !is_code_table {
                        tracing::error!("Invalid JumpTable Body Token: {:?}", self.current_token.kind);
                        return Err(ParserError {
                            kind: ParserErrorKind::InvalidTableBodyToken(self.current_token.kind.clone()),
                            hint: Some("Expected an identifier string.".to_string()),
                            spans: AstSpan(new_spans),
                            cursor: self.cursor,
                        });
                    }
                    let curr_spans = vec![self.current_token.span.clone()];
                    tracing::info!(target: "parser", "PARSING CODE TABLE BODY: [BYTES: {}]", hex::encode(&bytes));
                    self.consume();
                    statements.push(Statement { ty: StatementType::Code(bytes), span: AstSpan(curr_spans) });
                }
                TokenKind::BuiltinFunction(f) => {
                    if !is_code_table {
                        tracing::error!("Invalid JumpTable Body Token: {:?}", self.current_token.kind);
                        return Err(ParserError {
                            kind: ParserErrorKind::InvalidTableBodyToken(self.current_token.kind.clone()),
                            hint: Some("Expected an identifier string.".to_string()),
                            spans: AstSpan(new_spans),
                            cursor: self.cursor,
                        });
                    }
                    let mut curr_spans = vec![self.current_token.span.clone()];
                    self.consume();
                    let args = self.parse_builtin_args()?;
                    args.iter().for_each(|a| curr_spans.extend_from_slice(a.span().inner_ref()));
                    tracing::info!(target: "parser", "PARSING CODE TABLE BODY: [BUILTIN FN: {}({:?})]", f, args);
                    statements.push(Statement {
                        ty: StatementType::BuiltinFunctionCall(BuiltinFunctionCall {
                            kind: BuiltinFunctionKind::from(f.clone()),
                            args,
                            span: AstSpan(curr_spans.clone()),
                        }),
                        span: AstSpan(curr_spans),
                    });
                }
                TokenKind::OpenBracket => {
                    let (constant, const_span) = self.parse_constant_push()?;
                    tracing::info!(target: "parser", "PARSING CODE TABLE BODY: [CONSTANT: {}]", constant);
                    statements.push(Statement { ty: StatementType::Constant(constant), span: AstSpan(vec![const_span]) });
                }
                kind => {
                    tracing::error!("Invalid Table Body Token: {:?}", kind);
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidTableBodyToken(kind.clone()),
                        hint: Some("Expected an identifier string.".to_string()),
                        spans: AstSpan(new_spans),
                        cursor: self.cursor,
                    });
                }
            };
        }
        // consume close brace
        self.match_kind(TokenKind::CloseBrace)?;
        Ok(statements)
    }

    /// Parses a constant push.
    pub fn parse_constant_push(&mut self) -> Result<(String, Span), ParserError> {
        self.match_kind(TokenKind::OpenBracket)?;
        match self.current_token.kind.clone() {
            TokenKind::Ident(const_str) => {
                // Consume the Ident and Validate Close Bracket
                let iden_span = self.current_token.span.clone();
                self.consume();
                self.match_kind(TokenKind::CloseBracket)?;
                Ok((const_str, iden_span))
            }
            kind => {
                let new_spans = self.spans.clone();
                self.spans = vec![];
                Err(ParserError {
                    kind: ParserErrorKind::InvalidConstant(kind),
                    hint: None,
                    spans: AstSpan(new_spans),
                    cursor: self.cursor,
                })
            }
        }
    }

    /// Parses an argument call.
    ///
    /// ## Examples
    ///
    /// When an argument is called in Huff, it is wrapped in angle brackets like so:
    ///
    /// ```huff
    /// #define macro EXAMPLE_FUNCTION(error) = takes (0) returns (0) {
    ///     <error> jumpi
    /// }
    /// ```
    pub fn parse_arg_call(&mut self) -> Result<(String, Span), ParserError> {
        self.match_kind(TokenKind::LeftAngle)?;
        match self.current_token.kind.clone() {
            TokenKind::Ident(arg_str) => {
                let arg_call_span = self.current_token.span.clone();
                self.consume();
                self.match_kind(TokenKind::RightAngle)?;
                Ok((arg_str, arg_call_span))
            }
            kind => {
                let new_spans = self.spans.clone();
                self.spans = vec![];
                Err(ParserError {
                    kind: ParserErrorKind::InvalidArgCallIdent(kind),
                    hint: None,
                    spans: AstSpan(new_spans),
                    cursor: self.cursor,
                })
            }
        }
    }

    /// Parses whitespaces and newlines until none are left.
    pub fn parse_nl_or_whitespace(&mut self) -> Result<(), ParserError> {
        while self.check(TokenKind::Whitespace) {
            self.consume();
        }
        Ok(())
    }

    /// Parses the type of an argument.
    pub fn parse_arg_type(&mut self) -> Result<TokenKind, ParserError> {
        match self.current_token.kind.clone() {
            TokenKind::PrimitiveType(prim) => Ok(self.parse_primitive_type(prim)?),
            TokenKind::ArrayType(prim, _) => {
                // The trick is that when we parse the primitive type
                // of the array, it will consume the current token which is the ArrayType.
                // So we have to preserve the token before parsing (and matching thus consuming).
                let token = self.current_token.kind.clone();
                let _ = self.parse_primitive_type(prim);
                Ok(token)
            }
            kind => Err(ParserError {
                kind: ParserErrorKind::InvalidArgs(kind),
                hint: None,
                spans: AstSpan(vec![self.current_token.span.clone()]),
                cursor: self.cursor,
            }),
        }
    }

    /// Parses a primitive EVM type.
    /// Arrays of primitive types are not considered as primitive types themselves.
    pub fn parse_primitive_type(&mut self, prim: PrimitiveEVMType) -> Result<TokenKind, ParserError> {
        match prim {
            PrimitiveEVMType::Uint(size) => {
                if !(8..=256).contains(&size) || size % 8 != 0 {
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidUint256(size),
                        hint: None,
                        spans: AstSpan(vec![self.current_token.span.clone()]),
                        cursor: self.cursor,
                    });
                }
                Ok(self.match_kind(self.current_token.kind.clone())?)
            }
            PrimitiveEVMType::Bytes(size) => {
                if !(1..=32).contains(&size) {
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidBytes(size),
                        hint: None,
                        spans: AstSpan(vec![self.current_token.span.clone()]),
                        cursor: self.cursor,
                    });
                }
                Ok(self.match_kind(self.current_token.kind.clone())?)
            }
            PrimitiveEVMType::Bool => Ok(self.match_kind(self.current_token.kind.clone())?),
            PrimitiveEVMType::Address => Ok(self.match_kind(self.current_token.kind.clone())?),
            PrimitiveEVMType::String => Ok(self.match_kind(self.current_token.kind.clone())?),
            PrimitiveEVMType::DynBytes => Ok(self.match_kind(self.current_token.kind.clone())?),
            PrimitiveEVMType::Int(size) => {
                if !(8..=256).contains(&size) || size % 8 != 0 {
                    return Err(ParserError {
                        kind: ParserErrorKind::InvalidInt(size),
                        hint: None,
                        spans: AstSpan(vec![self.current_token.span.clone()]),
                        cursor: self.cursor,
                    });
                }
                let curr_token_kind = self.current_token.kind.clone();
                self.consume();
                Ok(curr_token_kind)
            }
        }
    }
}
