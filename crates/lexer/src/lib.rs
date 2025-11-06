mod context_stack;

use context_stack::ContextStack;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::lexer_context::Context;
use huff_neo_utils::prelude::*;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashMap;
use std::iter::Enumerate;
use std::{iter::Peekable, str::Chars};
use tracing::{debug, error};

lazy_static! {
    static ref TOKEN: HashMap<String, TokenKind> = HashMap::from_iter(vec![
        (TokenKind::Macro.to_string(), TokenKind::Macro),
        (TokenKind::Fn.to_string(), TokenKind::Fn),
        (TokenKind::Test.to_string(), TokenKind::Test),
        (TokenKind::Function.to_string(), TokenKind::Function),
        (TokenKind::Constant.to_string(), TokenKind::Constant),
        (TokenKind::Error.to_string(), TokenKind::Error),
        (TokenKind::Takes.to_string(), TokenKind::Takes),
        (TokenKind::Returns.to_string(), TokenKind::Returns),
        (TokenKind::Event.to_string(), TokenKind::Event),
        (TokenKind::NonPayable.to_string(), TokenKind::NonPayable),
        (TokenKind::Payable.to_string(), TokenKind::Payable),
        (TokenKind::Indexed.to_string(), TokenKind::Indexed),
        (TokenKind::View.to_string(), TokenKind::View),
        (TokenKind::Pure.to_string(), TokenKind::Pure),
        // First check for packed jump table
        (TokenKind::JumpTablePacked.to_string(), TokenKind::JumpTablePacked),
        // Match with jump table if not
        (TokenKind::JumpTable.to_string(), TokenKind::JumpTable),
        (TokenKind::CodeTable.to_string(), TokenKind::CodeTable),
    ]);
}

// For constants only max 32 Bytes is allowed for hex string starting with 0x. 2 + 64 = 66 characters
const MAX_HEX_LITERAL_LENGTH: usize = 66;

/// ## Lexer
///
/// The lexer encapsulated in a struct.
pub struct Lexer<'a> {
    /// The source code as peekable chars.
    /// WARN: SHOULD NEVER BE MODIFIED!
    pub chars: Peekable<Enumerate<Chars<'a>>>,
    /// The current position in the source code.
    position: usize,
    /// The previous lexed Token.
    /// NOTE: Cannot be a whitespace.
    pub lookback: Option<Token>,
    /// Bool indicating if we have reached EOF
    pub eof: bool,
    /// Current context stack.
    context_stack: ContextStack,
    /// The raw source code.
    pub source: FullFileSource<'a>,
}

pub type TokenResult = Result<Token, LexicalError>;

impl<'a> Lexer<'a> {
    pub fn new(source: FullFileSource<'a>) -> Self {
        Lexer {
            chars: source.source.chars().enumerate().peekable(),
            position: 0,
            lookback: None,
            eof: false,
            context_stack: ContextStack::new(),
            source,
        }
    }

    /// Consumes the next character
    pub fn consume(&mut self) -> Option<char> {
        let (index, c) = self.chars.next()?;
        self.position = index;
        Some(c)
    }

    /// Try to peek at the next character from the source
    pub fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn next_token(&mut self) -> TokenResult {
        if let Some(ch) = self.consume() {
            let token = match ch {
                '/' => {
                    let mut comment_string = String::new();
                    let start = self.position;
                    comment_string.push(ch);
                    if let Some(ch2) = self.peek() {
                        match ch2 {
                            '/' => {
                                // Consume until newline
                                comment_string.push(ch2);
                                let (comment_string, start, end) = self.eat_while(Some(ch), |c| c != '\n');
                                Ok(TokenKind::Comment(comment_string).into_token_with_span(self.source.relative_span_by_pos(start, end)))
                            }
                            '*' => {
                                // ref: https://github.com/rust-lang/rust/blob/900c3540378c8422b8087ffa3db60fa6c8abfcad/compiler/rustc_lexer/src/lib.rs#L474
                                let c = self.consume();
                                comment_string.push(c.unwrap());
                                let mut depth = 1usize;
                                while let Some(c) = self.consume() {
                                    match c {
                                        '/' if self.peek() == Some('*') => {
                                            comment_string.push(c);
                                            let c2 = self.consume();
                                            comment_string.push(c2.unwrap());
                                            depth += 1;
                                        }
                                        '*' if self.peek() == Some('/') => {
                                            comment_string.push(c);
                                            let c2 = self.consume();
                                            comment_string.push(c2.unwrap());
                                            depth -= 1;
                                            if depth == 0 {
                                                // This block comment is closed, so for a
                                                // construction like "/* */ */"
                                                // there will be a successfully parsed block comment
                                                // "/* */"
                                                // and " */" will be processed separately.
                                                break;
                                            }
                                        }
                                        _ => {
                                            comment_string.push(c);
                                        }
                                    }
                                }

                                Ok(TokenKind::Comment(comment_string)
                                    .into_token_with_span(self.source.relative_span_by_pos(start, self.position + 1)))
                            }
                            _ => self.single_char_token(TokenKind::Div),
                        }
                    } else {
                        self.single_char_token(TokenKind::Div)
                    }
                }

                // # keywords
                '#' => {
                    let (word, start, end) = self.eat_while(Some(ch), |ch| ch.is_ascii_alphabetic());

                    let mut found_kind: Option<TokenKind> = None;

                    let keys = [TokenKind::Define, TokenKind::Include];
                    for kind in keys.into_iter() {
                        let key = kind.to_string();
                        if key == word {
                            found_kind = Some(kind);
                            break;
                        }
                    }

                    if let Some(kind) = found_kind {
                        Ok(kind.into_token_with_span(self.source.relative_span_by_pos(start, end)))
                    } else if self.context_stack.top() == &Context::Global && self.peek().unwrap() == '[' {
                        Ok(TokenKind::Pound.into_token_with_span(self.source.relative_span_by_pos(self.position, self.position + 1)))
                    } else {
                        // Otherwise we don't support # prefixed identifiers
                        error!(target: "lexer", "INVALID '#' CHARACTER USAGE in context {:?}", self.context_stack.top());
                        return Err(LexicalError::new(
                            LexicalErrorKind::InvalidCharacter('#'),
                            self.source.relative_span_by_pos(self.position, self.position + 1),
                        ));
                    }
                }
                // Alphabetical characters
                ch if ch.is_alphabetic() || ch.eq(&'_') => {
                    let (word, start, mut end) = self.eat_while(Some(ch), |c| c.is_alphanumeric() || c == '_');

                    let mut found_kind: Option<TokenKind> = None;
                    if self.context_stack.top() != &Context::MacroBody
                        && let Some(kind) = TOKEN.get(&word)
                    {
                        debug!(target: "lexer", "FOUND KEYWORD OUTSIDE OF MacroBody '{:?}'", kind);
                        found_kind = Some(kind.clone());
                    }

                    // Check to see if the found kind is, in fact, a keyword and not the name of
                    // a function. If it is, set `found_kind` to `None` so that it is set to a
                    // `TokenKind::Ident` in the following control flow.
                    if !self.check_keyword_rules(&found_kind) {
                        debug!(target: "lexer", "RESET KEYWORD TO NONE '{:?}'", found_kind);
                        found_kind = None;
                    }

                    // Set the context based on the found token kind
                    if let Some(kind) = &found_kind {
                        let mut new_context = None;
                        match kind {
                            TokenKind::Macro | TokenKind::Fn | TokenKind::Test => {
                                debug!(target: "lexer", "PUSH CONTEXT MacroDefinition");
                                new_context = Some(Context::MacroDefinition);
                            }
                            TokenKind::Function | TokenKind::Event | TokenKind::Error => {
                                debug!(target: "lexer", "PUSH CONTEXT Abi");
                                new_context = Some(Context::Abi);
                            }
                            TokenKind::Constant => {
                                debug!(target: "lexer", "PUSH CONTEXT Constant");
                                new_context = Some(Context::Constant);
                            }
                            TokenKind::CodeTable => {
                                debug!(target: "lexer", "PUSH CONTEXT CodeTableBody");
                                new_context = Some(Context::CodeTableBody);
                            }
                            _ => (),
                        }

                        // For some contexts we have no terminal token, so we need to pop the top item
                        if new_context.is_some() && self.context_stack.top() != &Context::Global {
                            debug!(target: "lexer", "POP CONTEXT {:?}", self.context_stack.top());
                            self.context_stack.pop(1).map_err(|_| {
                                LexicalError::new(
                                    LexicalErrorKind::StackUnderflow,
                                    self.source.relative_span_by_pos(self.position, self.position + 1),
                                )
                            })?;
                        }
                        // Verify that the context is correct
                        if new_context.is_some() && self.context_stack.top() != &Context::Global {
                            return Err(LexicalError::new(
                                LexicalErrorKind::UnexpectedContext(self.context_stack.top().clone()),
                                self.source.relative_span_by_pos(self.position, self.position + 1),
                            ));
                        }
                        // Push the new context
                        if let Some(new_context) = new_context {
                            self.context_stack.push(new_context);
                        }
                    }

                    // Check for free storage pointer builtin
                    if word == "FREE_STORAGE_POINTER" {
                        // Consume the parenthesis following the FREE_STORAGE_POINTER
                        // Note: This will consume `FREE_STORAGE_POINTER)` or
                        // `FREE_STORAGE_POINTER(` as well
                        if let Some('(') = self.peek() {
                            self.consume();
                        }
                        if let Some(')') = self.peek() {
                            self.consume();
                        }
                        end += 2;
                        debug!(target: "lexer", "FOUND FREE_STORAGE_POINTER");
                        found_kind = Some(TokenKind::FreeStoragePointer);
                    }

                    // Check for __NOOP builtin constant
                    if word == "__NOOP"
                        && matches!(
                            self.context_stack.top(),
                            &Context::MacroBody | &Context::ForLoopBody | &Context::Constant | &Context::MacroArgs
                        )
                    {
                        debug!(target: "lexer", "FOUND __NOOP");
                        found_kind = Some(TokenKind::Noop);
                    }

                    if let Some(':') = self.peek() {
                        debug!(target: "lexer", "FOUND LABEL '{:?}'", word);
                        found_kind = Some(TokenKind::Label(word.clone()));
                    }

                    // Syntax sugar: true evaluates to 0x01, false evaluates to 0x00
                    if matches!(self.context_stack.top(), &Context::MacroBody | &Context::ForLoopBody | &Context::Constant)
                        && !self.checked_lookback(TokenKind::Constant) // allow to use `true` and `false` as identifiers
                        && matches!(word.as_str(), "true" | "false")
                    {
                        debug!(target: "lexer", "FOUND BOOL '{:?}'", word);
                        found_kind = Some(TokenKind::Literal(str_to_bytes32(if word.as_str() == "true" { "1" } else { "0" })));
                        self.eat_while(None, |c| c.is_alphanumeric());
                    }

                    // Check for loop keywords in MacroBody or ForLoopBody context
                    if matches!(self.context_stack.top(), &Context::MacroBody | &Context::ForLoopBody) && found_kind.is_none() {
                        match word.as_str() {
                            "for" => {
                                debug!(target: "lexer", "FOUND FOR KEYWORD");
                                found_kind = Some(TokenKind::For);
                            }
                            "in" => {
                                debug!(target: "lexer", "FOUND IN KEYWORD");
                                found_kind = Some(TokenKind::In);
                            }
                            "step" => {
                                debug!(target: "lexer", "FOUND STEP KEYWORD");
                                found_kind = Some(TokenKind::Step);
                            }
                            "if" => {
                                debug!(target: "lexer", "FOUND IF KEYWORD");
                                found_kind = Some(TokenKind::If);
                            }
                            "else" => {
                                debug!(target: "lexer", "FOUND ELSE KEYWORD");
                                found_kind = Some(TokenKind::Else);
                            }
                            _ => {}
                        }
                    }

                    if matches!(self.context_stack.top(), &Context::MacroBody | &Context::ForLoopBody)
                        && found_kind.is_none()
                        && let Some(o) = OPCODES_MAP.get(&word)
                    {
                        debug!(target: "lexer", "FOUND OPCODE '{:?}'", o);
                        found_kind = Some(TokenKind::Opcode(o.to_owned()));
                    }

                    if self.context_stack.top() == &Context::AbiArgs {
                        self.eat_abi_args(ch, &word, start, &mut end, &mut found_kind);
                    }

                    let kind = if let Some(kind) = found_kind {
                        kind
                    } else if matches!(
                        self.context_stack.top(),
                        &Context::MacroBody
                            | &Context::BuiltinFunction
                            | &Context::CodeTableBody
                            | &Context::Constant
                            | &Context::MacroArgs
                    ) && BuiltinFunctionKind::try_from(&word).is_ok()
                    {
                        TokenKind::BuiltinFunction(word)
                    } else {
                        TokenKind::Ident(word)
                    };

                    Ok(kind.into_token_with_span(self.source.relative_span_by_pos(start, end)))
                }
                // If it's the start of a hex literal
                ch if ch == '0' && self.peek().unwrap() == 'x' => self.eat_hex_digit(ch),
                '=' => {
                    // Check if next char is also '=' for EqualEqual (==)
                    if self.peek() == Some('=') {
                        let start = self.position;
                        self.consume(); // consume first '='
                        self.consume(); // consume second '='
                        let end = self.position;
                        Ok(TokenKind::EqualEqual.into_token_with_span(self.source.relative_span_by_pos(start, end)))
                    } else {
                        self.single_char_token(TokenKind::Assign)
                    }
                }
                '!' => {
                    // Check if next char is '=' for NotEqual (!=)
                    if self.peek() == Some('=') {
                        let start = self.position;
                        self.consume(); // consume '!'
                        self.consume(); // consume '='
                        let end = self.position;
                        Ok(TokenKind::NotEqual.into_token_with_span(self.source.relative_span_by_pos(start, end)))
                    } else {
                        // '!' by itself is logical NOT
                        self.single_char_token(TokenKind::Not)
                    }
                }
                '(' => {
                    match self.context_stack.top() {
                        Context::Abi => {
                            debug!(target: "lexer", "PUSH CONTEXT AbiArgs");
                            self.context_stack.push(Context::AbiArgs)
                        }
                        Context::MacroBody | Context::ForLoopBody => match self.lookback.as_ref().unwrap().kind {
                            TokenKind::BuiltinFunction(_) => {
                                debug!(target: "lexer", "PUSH CONTEXT BuiltinFunction");
                                self.context_stack.push(Context::BuiltinFunction)
                            }
                            TokenKind::For => {
                                // Don't push MacroArgs context for `for(...)` - keep current context
                                // so that loop keywords like `in` and `step` are recognized
                                debug!(target: "lexer", "KEEP CONTEXT for 'for' loop");
                            }
                            _ => {
                                debug!(target: "lexer", "PUSH CONTEXT MacroArgs");
                                self.context_stack.push(Context::MacroArgs)
                            }
                        },
                        _ => {}
                    }
                    self.single_char_token(TokenKind::OpenParen)
                }
                ')' => {
                    if matches!(self.context_stack.top(), &Context::AbiArgs | &Context::BuiltinFunction | &Context::MacroArgs) {
                        debug!(target: "lexer", "POP CONTEXT AbiArgs OR BuiltinFunction OR MacroArgs");
                        self.context_stack.pop(1).map_err(|_| {
                            LexicalError::new(
                                LexicalErrorKind::StackUnderflow,
                                self.source.relative_span_by_pos(self.position, self.position + 1),
                            )
                        })?
                    }
                    self.single_char_token(TokenKind::CloseParen)
                }
                '[' => self.single_char_token(TokenKind::OpenBracket),
                ']' => self.single_char_token(TokenKind::CloseBracket),
                '{' => {
                    // Push ForLoopBody context in MacroBody or ForLoopBody context for nested braces
                    if matches!(self.context_stack.top(), &Context::MacroBody | &Context::ForLoopBody) {
                        debug!(target: "lexer", "PUSH CONTEXT ForLoopBody");
                        self.context_stack.push(Context::ForLoopBody);
                    }

                    if self.context_stack.top() == &Context::MacroDefinition {
                        // New stack: Global -> MacroBody
                        debug!(target: "lexer", "REPLACE CONTEXT MacroDefinition to MacroBody");
                        self.context_stack.replace(Context::MacroBody);
                    }
                    self.single_char_token(TokenKind::OpenBrace)
                }
                '}' => {
                    if matches!(self.context_stack.top(), &Context::ForLoopBody | &Context::MacroBody | &Context::CodeTableBody) {
                        debug!(target: "lexer", "POP CONTEXT {:?}", self.context_stack.top());
                        self.context_stack.pop(1).map_err(|_| {
                            LexicalError::new(
                                LexicalErrorKind::StackUnderflow,
                                self.source.relative_span_by_pos(self.position, self.position + 1),
                            )
                        })?;
                    }
                    self.single_char_token(TokenKind::CloseBrace)
                }
                '+' => self.single_char_token(TokenKind::Add),
                '-' => self.single_char_token(TokenKind::Sub),
                '*' => self.single_char_token(TokenKind::Mul),
                '%' => self.single_char_token(TokenKind::Mod),
                '<' => {
                    // Check if next char is '=' for LessEqual (<=)
                    if self.peek() == Some('=') {
                        let start = self.position;
                        self.consume(); // consume '<'
                        self.consume(); // consume '='
                        let end = self.position;
                        Ok(TokenKind::LessEqual.into_token_with_span(self.source.relative_span_by_pos(start, end)))
                    } else {
                        self.single_char_token(TokenKind::LeftAngle)
                    }
                }
                '>' => {
                    // Check if next char is '=' for GreaterEqual (>=)
                    if self.peek() == Some('=') {
                        let start = self.position;
                        self.consume(); // consume '>'
                        self.consume(); // consume '='
                        let end = self.position;
                        Ok(TokenKind::GreaterEqual.into_token_with_span(self.source.relative_span_by_pos(start, end)))
                    } else {
                        self.single_char_token(TokenKind::RightAngle)
                    }
                }
                // NOTE: TokenKind::Div is lexed further up since it overlaps with comment
                ':' => self.single_char_token(TokenKind::Colon),
                '.' => {
                    // Check if next char is also '.' for DoubleDot (..)
                    if self.peek() == Some('.') {
                        self.consume(); // consume the second '.'
                        Ok(TokenKind::DoubleDot
                            .into_token_with_span(self.source.relative_span_by_pos(self.position - 1, self.position + 1)))
                    } else {
                        // Single dot is not supported
                        error!(target: "lexer", "UNSUPPORTED TOKEN '.'");
                        return Err(LexicalError::new(
                            LexicalErrorKind::InvalidCharacter('.'),
                            self.source.relative_span_by_pos(self.position, self.position),
                        ));
                    }
                }
                // identifiers
                ',' => self.single_char_token(TokenKind::Comma),
                '0'..='9' => self.eat_digit(ch),
                // Lexes Spaces and Newlines as Whitespace
                ch if ch.is_ascii_whitespace() => {
                    let (_, start, end) = self.eat_while(Some(ch), |c| c.is_whitespace());
                    Ok(TokenKind::Whitespace.into_token_with_span(self.source.relative_span_by_pos(start, end)))
                }
                // String literals. String literals can also be wrapped by single quotes
                '"' | '\'' => Ok(self.eat_string_literal()),
                ch => {
                    error!(target: "lexer", "UNSUPPORTED TOKEN '{}'", ch);
                    return Err(LexicalError::new(
                        LexicalErrorKind::InvalidCharacter(ch),
                        self.source.relative_span_by_pos(self.position, self.position),
                    ));
                }
            }?;

            if token.kind != TokenKind::Whitespace {
                self.lookback = Some(token.clone());
            }

            Ok(token)
        } else {
            self.eof = true;
            let eof_pos = self.source.source.len();
            Ok(Token { kind: TokenKind::Eof, span: self.source.relative_span_by_pos(eof_pos, eof_pos) })
        }
    }

    fn eat_abi_args(&mut self, ch: char, word: &String, start: usize, end: &mut usize, found_kind: &mut Option<TokenKind>) {
        let curr_char = self.peek().unwrap();
        if !['(', ')'].contains(&curr_char) {
            let (partial_raw_type, _, abi_args_end) = self.eat_while(Some(ch), |c| c.is_alphanumeric() || c == '[' || c == ']');
            let raw_type = word.clone() + &partial_raw_type[1..];

            if raw_type == TokenKind::Calldata.to_string() {
                debug!(target: "lexer", "FOUND CALLED DATA '{:?}'", raw_type);
                *found_kind = Some(TokenKind::Calldata);
            } else if raw_type == TokenKind::Memory.to_string() {
                debug!(target: "lexer", "FOUND MEMORY '{:?}'", raw_type);
                *found_kind = Some(TokenKind::Memory);
            } else if raw_type == TokenKind::Storage.to_string() {
                debug!(target: "lexer", "FOUND STORAGE '{:?}'", raw_type);
                *found_kind = Some(TokenKind::Storage);
            } else if EVM_TYPE_ARRAY_REGEX.is_match(&raw_type) {
                // split to get array size and type
                // TODO: support multi-dimensional arrays
                let words: Vec<String> = Regex::new(r"\[").unwrap().split(&raw_type).map(|x| x.replace(']', "")).collect();
                let mut size_vec: Vec<usize> = Vec::new();
                // go over all array sizes
                let sizes = words.get(1..words.len()).unwrap();
                for size in sizes.iter() {
                    match size.is_empty() {
                        true => size_vec.push(0),
                        false => {
                            let arr_size: usize = size
                                .parse::<usize>()
                                .map_err(|_| {
                                    let err = LexicalError {
                                        kind: LexicalErrorKind::InvalidArraySize(words[1].clone()),
                                        span: self.source.relative_span_by_pos(start, *end),
                                    };
                                    error!(target: "lexer", "{}", format!("{err:?}"));
                                    err
                                })
                                .unwrap();
                            size_vec.push(arr_size);
                        }
                    }
                }
                let primitive = PrimitiveEVMType::try_from(&words[0]);
                if let Ok(primitive) = primitive {
                    debug!(target: "lexer", "FOUND ARRAY '{:?}'", raw_type);
                    *found_kind = Some(TokenKind::ArrayType(primitive, size_vec));
                } else {
                    let err = LexicalError {
                        kind: LexicalErrorKind::InvalidPrimitiveType(words[0].clone()),
                        span: self.source.relative_span_by_pos(start, *end),
                    };
                    error!(target: "lexer", "{}", format!("{err:?}"));
                }
            } else {
                // We don't want to consider any argument names or the "indexed"
                // keyword here.
                let primitive = PrimitiveEVMType::try_from(word);
                if let Ok(primitive) = primitive {
                    debug!(target: "lexer", "FOUND PRIMITIVE '{:?}'", raw_type);
                    *found_kind = Some(TokenKind::PrimitiveType(primitive));
                }
            }
            *end = abi_args_end;
        } else {
            // We don't want to consider any argument names or the "indexed"
            // keyword here.
            let primitive = PrimitiveEVMType::try_from(word);
            if let Ok(primitive) = primitive {
                debug!(target: "lexer", "FOUND PRIMITIVE '{:?}'", word);
                *found_kind = Some(TokenKind::PrimitiveType(primitive));
            }
        }
    }

    fn single_char_token(&self, token_kind: TokenKind) -> TokenResult {
        Ok(token_kind.into_token_with_span(self.source.relative_span_by_pos(self.position, self.position + 1)))
    }

    /// Keeps consuming tokens as long as the predicate is satisfied
    fn eat_while<F: Fn(char) -> bool>(&mut self, initial_char: Option<char>, predicate: F) -> (String, usize, usize) {
        let start = self.position;

        // This function is only called when we want to continue consuming a character of the same
        // type. For example, we see a digit, and we want to consume the whole integer.
        // Therefore, the current character which triggered this function will need to be appended.
        let mut word = String::new();
        if let Some(init_char) = initial_char {
            word.push(init_char)
        }

        // Keep checking that we are not at the EOF
        while let Some(peek_char) = self.peek() {
            // Then check for the predicate, if predicate matches append char and increment the
            // cursor If not, return word. The next character will be analyzed on the
            // next iteration of next_token, Which will increment the cursor
            if !predicate(peek_char) {
                // Position currently points to the last consumed character
                // We need to add its byte length to get the end position
                let end_position = if word.is_empty() { self.position } else { self.position + word.chars().last().unwrap().len_utf8() };
                return (word, start, end_position);
            }
            word.push(peek_char);

            // If we arrive at this point, then the char has been added to the word and we should
            // increment the cursor
            self.consume();
        }

        // After consuming all characters, position points to the last consumed character
        // We need to add the length of that last character to get the end position
        let end_position = if word.is_empty() {
            self.position
        } else {
            // Get the byte length of the last character and add it to position
            self.position + word.chars().last().unwrap().len_utf8()
        };

        (word, start, end_position)
    }

    fn eat_digit(&mut self, initial_char: char) -> TokenResult {
        let (integer_str, start, end) = self.eat_while(Some(initial_char), |ch| ch.is_ascii_digit());

        let integer = integer_str.parse().unwrap();
        let integer_token = TokenKind::Num(integer);

        Ok(Token { kind: integer_token, span: self.source.relative_span_by_pos(start, end) })
    }

    fn eat_hex_digit(&mut self, initial_char: char) -> TokenResult {
        let (integer_str, start, end) = self.eat_while(Some(initial_char), |ch| ch.is_ascii_hexdigit() | (ch == 'x'));
        if integer_str.matches('x').count() != 1 {
            return Err(LexicalError::new(
                LexicalErrorKind::InvalidHexLiteral(integer_str.clone()),
                self.source.relative_span_by_pos(start, end),
            ));
        }

        let kind = if matches!(self.context_stack.top(), &Context::CodeTableBody | &Context::Constant) {
            // In code tables, or constant values the bytecode provided is of arbitrary length. We pass
            // the code as an Ident, and parse it later.
            if self.context_stack.top() == &Context::Constant && integer_str.len() > MAX_HEX_LITERAL_LENGTH {
                return Err(LexicalError::new(
                    LexicalErrorKind::HexLiteralTooLong(integer_str.clone()),
                    self.source.relative_span_by_pos(start, end),
                ));
            }
            let hex_string = format_even_bytes(integer_str[2..].to_lowercase());
            TokenKind::Bytes(hex_string)
        } else {
            if integer_str.len() > MAX_HEX_LITERAL_LENGTH {
                return Err(LexicalError::new(
                    LexicalErrorKind::HexLiteralTooLong(integer_str.clone()),
                    self.source.relative_span_by_pos(start, end),
                ));
            }
            TokenKind::Literal(str_to_bytes32(integer_str[2..].as_ref()))
        };

        Ok(Token { kind, span: self.source.relative_span_by_pos(start, end) })
    }

    /// Skips white space. They are not significant in the source language
    fn eat_whitespace(&mut self) -> (String, usize, usize) {
        self.eat_while(None, |ch| ch.is_whitespace())
    }

    fn eat_string_literal(&mut self) -> Token {
        let (str_literal, start_span, end_span) = self.eat_while(None, |ch| ch != '"' && ch != '\'');
        let str_literal_token = TokenKind::Str(str_literal);
        self.consume(); // Advance past the closing quote
        str_literal_token.into_token_with_span(self.source.relative_span_by_pos(start_span, end_span + 1))
    }

    /// Checks the previous token kind against the input.
    pub fn checked_lookback(&self, kind: TokenKind) -> bool {
        self.lookback.as_ref().and_then(|t| if t.kind == kind { Some(true) } else { None }).is_some()
    }

    /// Check if a given keyword follows the keyword rules in the `source`. If not, it is a
    /// `TokenKind::Ident`.
    ///
    /// Rules:
    /// - The `macro`, `fn`, `test`, `function`, `constant`, `event`, `jumptable`,
    ///   `jumptable__packed`, and `table` keywords must be preceded by a `#define` keyword.
    /// - The `takes` keyword must be preceded by an assignment operator: `=`.
    /// - The `nonpayable`, `payable`, `view`, and `pure` keywords must be preceeded by one of these
    ///   keywords or a close paren.
    /// - The `returns` keyword must be succeeded by an open parenthesis and must *not* be succeeded
    ///   by a colon or preceded by the keyword `function`
    pub fn check_keyword_rules(&mut self, found_kind: &Option<TokenKind>) -> bool {
        match found_kind {
            Some(TokenKind::Macro)
            | Some(TokenKind::Fn)
            | Some(TokenKind::Test)
            | Some(TokenKind::Function)
            | Some(TokenKind::Constant)
            | Some(TokenKind::Error)
            | Some(TokenKind::Event)
            | Some(TokenKind::JumpTable)
            | Some(TokenKind::JumpTablePacked)
            | Some(TokenKind::CodeTable) => self.checked_lookback(TokenKind::Define),
            Some(TokenKind::NonPayable) | Some(TokenKind::Payable) | Some(TokenKind::View) | Some(TokenKind::Pure) => {
                let keys = [TokenKind::NonPayable, TokenKind::Payable, TokenKind::View, TokenKind::Pure, TokenKind::CloseParen];
                for key in keys {
                    if self.checked_lookback(key) {
                        return true;
                    }
                }
                false
            }
            Some(TokenKind::Takes) => self.checked_lookback(TokenKind::Assign),
            Some(TokenKind::Returns) => {
                self.eat_whitespace();
                // Allow for loose and tight syntax (e.g. `returns   (0)`, `returns(0)`, ...)
                self.peek().unwrap_or(')') == '(' && !self.checked_lookback(TokenKind::Function)
            }
            _ => true,
        }
    }

    /// Lex all imports
    /// Example import: `// #include "./Utils.huff"`
    pub fn lex_imports(source: &str) -> Vec<String> {
        let mut imports = vec![];
        let mut peekable_source = source.chars().peekable();
        let mut include_chars_iterator = "#include".chars().peekable();
        while peekable_source.peek().is_some() {
            while let Some(nc) = peekable_source.next() {
                if nc.eq(&'/')
                    && let Some(nnc) = peekable_source.peek()
                {
                    if nnc.eq(&'/') {
                        // Iterate until newline
                        while let Some(lc) = &peekable_source.next() {
                            if lc.eq(&'\n') {
                                break;
                            }
                        }
                    } else if nnc.eq(&'*') {
                        // Iterate until '*/'
                        while let Some(lc) = peekable_source.next() {
                            if lc.eq(&'*')
                                && let Some(llc) = peekable_source.peek()
                                && *llc == '/'
                            {
                                break;
                            }
                        }
                    }
                }
                if include_chars_iterator.peek().is_none() {
                    // Reset the include chars iterator
                    include_chars_iterator = "#include".chars().peekable();

                    // Skip over whitespace
                    while peekable_source.peek().is_some() {
                        if !peekable_source.peek().unwrap().is_whitespace() {
                            break;
                        } else {
                            peekable_source.next();
                        }
                    }

                    // Then we should have an import path between quotes
                    #[allow(clippy::collapsible_match)]
                    if let Some(char) = peekable_source.peek() {
                        match char {
                            '"' | '\'' => {
                                peekable_source.next();
                                let mut import = String::new();
                                while peekable_source.peek().is_some() {
                                    if let Some(c) = peekable_source.next() {
                                        if matches!(c, '"' | '\'') {
                                            imports.push(import);
                                            break;
                                        } else {
                                            import.push(c);
                                        }
                                    }
                                }
                            }
                            _ => { /* Ignore non-include tokens */ }
                        }
                    }
                } else if nc.ne(&include_chars_iterator.next().unwrap()) {
                    include_chars_iterator = "#include".chars().peekable();
                    break;
                }
            }
        }
        imports
    }
}

impl Iterator for Lexer<'_> {
    type Item = TokenResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof { None } else { Some(self.next_token()) }
    }
}
