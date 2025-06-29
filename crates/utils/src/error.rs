use crate::ast::span::AstSpan;
use crate::file::span::{Span, Spanned};
use crate::file::unpack_files::UnpackError;
use crate::lexer_context::Context;
use crate::{
    prelude::{Opcode, parse_extension},
    report::{Report, Reporter},
    token::TokenKind,
};
use std::{ffi::OsString, fmt, io::Write};

/// A Parser Error
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct ParserError {
    /// The type of Parser Error
    pub kind: ParserErrorKind,
    /// Hints about the error
    pub hint: Option<String>,
    /// A collection of spans the Parser Error crosses
    pub spans: AstSpan,
    /// Line number where the error was seen
    pub cursor: usize,
}

/// A Type of Parser Error
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ParserErrorKind {
    /// An invalid literal was passed to a push opcode
    InvalidPush(Opcode),
    /// Unexpected type
    UnexpectedType(TokenKind),
    /// Argument name is a reserved evm primitive type keyword
    InvalidTypeAsArgumentName(TokenKind),
    /// Invalid definition
    InvalidDefinition(TokenKind),
    /// Invalid constant value
    InvalidConstantValue(TokenKind),
    /// Unexpected token in macro body
    InvalidTokenInMacroBody(TokenKind),
    /// Unexpected token in label definition
    InvalidTokenInLabelDefinition(TokenKind),
    /// Unexpected Single Arg
    InvalidSingleArg(TokenKind),
    /// Unexpected Table Body Token
    InvalidTableBodyToken(TokenKind),
    /// Invalid constant
    InvalidConstant(TokenKind),
    /// Unexpected Arg Call Token
    InvalidArgCallIdent(TokenKind),
    /// Invalid name (macro, event, function, constant)
    InvalidName(TokenKind),
    /// Invalid arguments
    InvalidArgs(TokenKind),
    /// Invalid Uint256 Size
    InvalidUint256(usize),
    /// Invalid Bytes
    InvalidBytes(usize),
    /// Invalid Int
    InvalidInt(usize),
    /// Invalid macro call arguments
    InvalidMacroArgs(TokenKind),
    /// Invalid return arguments
    InvalidReturnArgs,
    /// Invalid import path
    InvalidImportPath(String),
    /// Invalid decorator flag
    InvalidDecoratorFlag(String),
    /// Invalid decorator flag argument
    InvalidDecoratorFlagArg(TokenKind),
    /// Duplicate label
    DuplicateLabel(String),
    /// Duplicate MACRO
    DuplicateMacro(String),
    /// Invalid code table statement
    InvalidTableStatement(String),
}

/// A Lexing Error
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LexicalError {
    /// The kind of error
    pub kind: LexicalErrorKind,
    /// The span where the error occurred
    pub span: Span,
}

impl LexicalError {
    /// Public associated function to instatiate a new LexicalError.
    pub fn new(kind: LexicalErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// A Lexical Error Kind
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LexicalErrorKind {
    /// Unexpected end of file
    UnexpectedEof,
    /// Invalid character
    InvalidCharacter(char),
    /// Invalid hex literal
    HexLiteralTooLong(String),
    /// Invalid Hex Literal
    InvalidHexLiteral(String),
    /// Invalid Array Size
    /// String param expected to be usize parsable
    InvalidArraySize(String),
    /// Invalid Primitive EVM Type
    InvalidPrimitiveType(String),
    /// Stack Underflow
    StackUnderflow,
    /// Unexpected Context
    UnexpectedContext(Context),
}

impl Spanned for LexicalError {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl<W: Write> Report<W> for LexicalError {
    fn report(&self, f: &mut Reporter<'_, W>) -> std::io::Result<()> {
        match &self.kind {
            LexicalErrorKind::InvalidCharacter(ch) => write!(f.out, "Invalid character '{ch}'"),
            LexicalErrorKind::UnexpectedEof => write!(f.out, "Found unexpected EOF"),
            LexicalErrorKind::InvalidArraySize(str) => {
                write!(f.out, "Invalid array size: '{str}'")
            }
            LexicalErrorKind::InvalidPrimitiveType(str) => {
                write!(f.out, "Invalid Primitive EVM Type '{str}'")
            }
            LexicalErrorKind::HexLiteralTooLong(str) => {
                write!(f.out, "Hex literal has more than 32 bytes '{str}'")
            }
            LexicalErrorKind::InvalidHexLiteral(str) => {
                write!(f.out, "Invalid hex literal '{str}'")
            }
            LexicalErrorKind::StackUnderflow => write!(f.out, "Stack underflow"),
            LexicalErrorKind::UnexpectedContext(context) => write!(f.out, "Unexpected context: {context:?}"),
        }
    }
}

/// A Code Generation Error
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CodegenError {
    /// The kind of code generation error
    pub kind: CodegenErrorKind,
    /// An Optional Span where the error occured
    pub span: AstSpan,
    /// An Optional Token Kind
    pub token: Option<TokenKind>,
}

impl CodegenError {
    /// Public associated function to instatiate a new CodegenError.
    pub fn new(kind: CodegenErrorKind, spans: AstSpan, token: Option<TokenKind>) -> Self {
        Self { kind, span: spans, token }
    }
}

/// The Code Generation Error Kind
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CodegenErrorKind {
    /// Locking Error
    LockingError,
    /// Storage Pointers Not Derived
    StoragePointersNotDerived,
    /// Invalid Macro Body Statement
    InvalidMacroStatement,
    /// The Argument Definition is Missing
    MissingArgumentDefinition(String),
    /// The Macro Definition is Missing
    MissingMacroDefinition(String),
    /// The Function Interface is Missing
    MissingFunctionInterface(String),
    /// The Event Interface is Missing
    MissingEventInterface(String),
    /// Missing Constant Definition
    MissingConstantDefinition(String),
    /// Missing Error Definition
    MissingErrorDefinition(String),
    /// Abi Generation Failure
    AbiGenerationFailure,
    /// Unmatched Jump
    UnmatchedJumpLabels(Vec<String>),
    /// An IO Error
    IOError(String),
    /// ArgCall has an unknown type
    UnkownArgcallType,
    /// Missing Macro Invocation
    MissingMacroInvocation(String),
    /// Missing Macro Definition for Invocation
    InvalidMacroInvocation(String),
    /// Invalid Argument Count with macro name, expected and actual
    InvalidArgumentCount(String, usize, usize),
    /// Conversion Error for usize
    UsizeConversion(String),
    /// Invalid Arguments
    InvalidArguments(String),
    /// Invalid Hex String
    InvalidHex(String),
    /// Invalid Table Statement
    InvalidTableStatement(String),
    /// Invalid Code Length
    InvalidCodeLength(usize),
    /// Test Invocation
    TestInvocation(String),
    /// Incorrect dynamic argument index
    InvalidDynArgIndex,
    /// Missing Table Size
    MissingTableSize(String),
    /// Unsupported Builtin Function
    UnsupportedBuiltinFunction(String),
    /// Unsupported Statement Type
    UnsupportedStatementType(String),
}

impl Spanned for CodegenError {
    fn span(&self) -> Span {
        self.span[0].clone()
    }
}

impl<W: Write> Report<W> for CodegenError {
    fn report(&self, f: &mut Reporter<'_, W>) -> std::io::Result<()> {
        match &self.kind {
            CodegenErrorKind::LockingError => {
                write!(f.out, "Synchronisation Error - Please execute again!")
            }
            CodegenErrorKind::StoragePointersNotDerived => {
                write!(f.out, "Storage pointers not derived for AST!")
            }
            CodegenErrorKind::InvalidMacroStatement => write!(f.out, "Invalid Macro Statement!"),
            CodegenErrorKind::InvalidMacroInvocation(str) => {
                write!(f.out, "Missing Macro Definition for Invocation: \"{str}\"!")
            }
            CodegenErrorKind::InvalidArgumentCount(str, expected, actual) => {
                write!(f.out, "Invalid Argument Count for \"{str}\", expected {expected}, got {actual}!")
            }
            CodegenErrorKind::MissingArgumentDefinition(str) => {
                write!(f.out, "Missing Argument \"{str}\" Definition!")
            }
            CodegenErrorKind::MissingMacroDefinition(str) => {
                write!(f.out, "Missing Macro \"{str}\" Definition!")
            }
            CodegenErrorKind::MissingFunctionInterface(str) => {
                write!(f.out, "Missing Function Interface for \"{str}\"!")
            }
            CodegenErrorKind::MissingEventInterface(str) => {
                write!(f.out, "Missing Event Interface for \"{str}\"!")
            }
            CodegenErrorKind::MissingConstantDefinition(cd) => {
                write!(f.out, "Missing Constant Definition for \"{cd}\"!")
            }
            CodegenErrorKind::MissingErrorDefinition(ed) => {
                write!(f.out, "Missing Error Definition for \"{ed}\"!")
            }
            CodegenErrorKind::AbiGenerationFailure => write!(f.out, "Abi generation failure!"),
            CodegenErrorKind::UnmatchedJumpLabels(labels) => write!(f.out, "Unmatched jump labels: \"{}\"!", labels.join(", ")),
            CodegenErrorKind::IOError(ioe) => write!(f.out, "IO ERROR: {ioe:?}"),
            CodegenErrorKind::UnkownArgcallType => write!(f.out, "Unknown Argcall Type!"),
            CodegenErrorKind::MissingMacroInvocation(str) => {
                write!(f.out, "Missing Macro \"{str}\" Invocation!")
            }
            CodegenErrorKind::UsizeConversion(input) => {
                write!(f.out, "Usize Conversion Failed for \"{input}\"")
            }
            CodegenErrorKind::InvalidArguments(msg) => {
                write!(f.out, "Invalid arguments: \"{msg}\"")
            }
            CodegenErrorKind::InvalidHex(msg) => {
                write!(f.out, "Invalid hex string: \"{msg}\"")
            }
            CodegenErrorKind::InvalidTableStatement(msg) => {
                write!(f.out, "Invalid table statement: \"{msg}\"")
            }
            CodegenErrorKind::InvalidCodeLength(len) => {
                write!(f.out, "Invalid code length: {len}")
            }
            CodegenErrorKind::TestInvocation(msg) => {
                write!(f.out, "Test cannot be invoked: \"{msg}\"")
            }
            CodegenErrorKind::InvalidDynArgIndex => {
                write!(f.out, "Invalid Dynamic Constructor Argument Index")
            }
            CodegenErrorKind::MissingTableSize(table_name) => {
                write!(f.out, "Missing Table Size for table: \"{table_name}\"")
            }
            CodegenErrorKind::UnsupportedBuiltinFunction(bf) => {
                write!(f.out, "Unsupported Builtin Function: \"{bf}\"")
            }
            CodegenErrorKind::UnsupportedStatementType(st) => {
                write!(f.out, "Unsupported Statement Type: \"{st}\"")
            }
        }
    }
}

/// CompilerError
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompilerError {
    /// Failed to Lex Source
    LexicalError(LexicalError),
    /// File unpacking error
    FileUnpackError(UnpackError),
    /// Parsing Error
    ParserError(ParserError),
    /// Reading PathBuf Failed
    PathBufRead(OsString),
    /// Empty Import Path
    EmptyImportPath(OsString),
    /// Bytecode Generation Error
    CodegenError(CodegenError),
    /// Multiple Failed Compiles
    FailedCompiles(Vec<CompilerError>),
}

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CompilerError::LexicalError(le) => match &le.kind {
                LexicalErrorKind::UnexpectedEof => {
                    write!(f, "\nError: Unexpected End Of File {}{}\n", le.span.identifier(), le.span.source_seg())
                }
                LexicalErrorKind::InvalidCharacter(c) => {
                    write!(f, "\nError: Invalid Character: \"{}\" {}{}\n", c, le.span.identifier(), le.span.source_seg())
                }
                LexicalErrorKind::InvalidArraySize(a) => {
                    write!(f, "\nError: Invalid Array Size: \"{}\" {}{}\n", a, le.span.identifier(), le.span.source_seg())
                }
                LexicalErrorKind::InvalidPrimitiveType(ty) => {
                    write!(f, "\nError: Invalid Primitive Type: \"{}\" {}{}\n", ty, le.span.identifier(), le.span.source_seg())
                }
                LexicalErrorKind::HexLiteralTooLong(h) => {
                    write!(f, "\nError: Hex literal has more than 32 bytes: \"{}\" {}{}\n", h, le.span.identifier(), le.span.source_seg())
                }
                LexicalErrorKind::InvalidHexLiteral(h) => {
                    write!(f, "\nError: Invalid Hex literal: \"{}\" {}{}\n", h, le.span.identifier(), le.span.source_seg())
                }
                LexicalErrorKind::StackUnderflow => {
                    write!(f, "\nError: Stack Underflow {}{}\n", le.span.identifier(), le.span.source_seg())
                }
                LexicalErrorKind::UnexpectedContext(context) => {
                    write!(f, "\nError: Unexpected Context: {:?} {}{}\n", context, le.span.identifier(), le.span.source_seg())
                }
            },
            CompilerError::FileUnpackError(ue) => match ue {
                UnpackError::InvalidDirectory(id) => {
                    write!(f, "\nError: Invalid File Directory {id}\n")
                }
                UnpackError::UnsupportedExtension(unsupported) => {
                    write!(
                        f,
                        "\nError: Unsupported File Extension \"{}\"\n--> {}\n",
                        parse_extension(unsupported).unwrap_or(""),
                        unsupported
                    )
                }
                UnpackError::MissingFile(file) => {
                    write!(f, "\nError: File Not Found \"{file}\"\n")
                }
            },
            CompilerError::ParserError(pe) => match &pe.kind {
                ParserErrorKind::InvalidPush(op) => {
                    write!(f, "\nError at token {}: Invalid use of \"{:?}\" \n{}\n", pe.cursor, op, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::UnexpectedType(ut) => {
                    write!(f, "\nError at token {}: Unexpected Type: \"{}\" \n{}\n", pe.cursor, ut, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidTypeAsArgumentName(ut) => {
                    write!(
                        f,
                        "\nError at token {}: Unexpected Argument Name is an EVM Type: \"{}\" \n{}\n",
                        pe.cursor,
                        ut,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::InvalidDefinition(k) => {
                    write!(f, "\nError at token {}: Invalid Defintion \"{}\"\n{}\n", pe.cursor, k, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidConstantValue(cv) => {
                    write!(f, "\nError at token {}: Invalid Constant Value: \"{}\" \n{}\n", pe.cursor, cv, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidTokenInMacroBody(tmb) => {
                    write!(
                        f,
                        "\nError at token {}: Invalid Token In Macro Body: \"{}\" \n{}\n",
                        pe.cursor,
                        tmb,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::InvalidTokenInLabelDefinition(tlb) => {
                    write!(
                        f,
                        "\nError at token {}: Invalid Token In Label Defintiion: \"{}\" \n{}\n",
                        pe.cursor,
                        tlb,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::InvalidSingleArg(sa) => {
                    write!(f, "\nError at token {}: Invalid Argument: \"{}\" \n{}\n", pe.cursor, sa, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidTableBodyToken(tbt) => {
                    write!(
                        f,
                        "\nError at token {}: Invalid Token In Table Body: \"{}\" \n{}\n",
                        pe.cursor,
                        tbt,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::InvalidConstant(constant) => {
                    write!(f, "\nError at token {}: Invalid Constant: \"{}\" \n{}\n", pe.cursor, constant, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidArgCallIdent(aci) => {
                    write!(
                        f,
                        "\nError at token {}: Invalid Argument Call Identifier: \"{}\" \n{}\n",
                        pe.cursor,
                        aci,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::InvalidName(name) => {
                    write!(f, "\nError at token {}: Invalid Name: \"{}\" \n{}\n", pe.cursor, name, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidArgs(args) => {
                    write!(
                        f,
                        "\nError at token {}: Invalid Argument Type: \"{}\" \n{}\n",
                        pe.cursor,
                        args,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::InvalidUint256(v) => {
                    write!(f, "\nError at token {}: Invalid Uint256 Value: \"{}\" \n{}\n", pe.cursor, v, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidBytes(b) => {
                    write!(f, "\nError at token {}: Invalid Bytes Value: \"{}\" \n{}\n", pe.cursor, b, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidInt(i) => {
                    write!(f, "\nError at token {}: Invalid Int Value: \"{}\" \n{}\n", pe.cursor, i, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidMacroArgs(ma) => {
                    write!(
                        f,
                        "\nError at token {}: Invalid Macro Arguments: \"{}\" \n{}\n",
                        pe.cursor,
                        ma,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::InvalidReturnArgs => {
                    write!(f, "\nError at token {}: Invalid Return Arguments\n{}\n", pe.cursor, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidImportPath(ip) => {
                    write!(f, "\nError at token {}: Invalid Import Path: \"{}\" \n{}\n", pe.cursor, ip, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidDecoratorFlag(df) => {
                    write!(f, "\nError at token {}: Invalid Decorator Flag: \"{}\" \n{}\n", pe.cursor, df, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidDecoratorFlagArg(dfa) => {
                    write!(
                        f,
                        "\nError at token {}: Invalid Decorator Flag Argument: \"{}\" \n{}\n",
                        pe.cursor,
                        dfa,
                        pe.spans.error(pe.hint.as_ref())
                    )
                }
                ParserErrorKind::DuplicateLabel(label) => {
                    write!(f, "\nError: Duplicate label: \"{}\" \n{}\n", label, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::DuplicateMacro(mn) => {
                    write!(f, "\nError: Duplicate MACRO name found: \"{}\" \n{}\n", mn, pe.spans.error(pe.hint.as_ref()))
                }
                ParserErrorKind::InvalidTableStatement(statement_type) => {
                    write!(f, "\nError: Invalid Table Statement: \"{}\" \n{}\n", statement_type, pe.spans.error(pe.hint.as_ref()))
                }
            },
            CompilerError::PathBufRead(os_str) => {
                write!(f, "\nError: Invalid Import Path: \"{}\"", os_str.as_os_str().to_str().unwrap_or("<unknown import>"))
            }
            CompilerError::CodegenError(ce) => match &ce.kind {
                CodegenErrorKind::LockingError => {
                    write!(f, "\nError: Synchronisation Failure\n")
                }
                CodegenErrorKind::StoragePointersNotDerived => {
                    write!(f, "\nError: Storage Pointers Not Derived\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::InvalidMacroStatement => {
                    write!(f, "\nError: Invalid Macro Statement\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::InvalidArgumentCount(mn, expected, actual) => {
                    write!(
                        f,
                        "\nError: Invalid Argument Count for \"{}\", expected {}, got {}!\n{}",
                        mn,
                        expected,
                        actual,
                        ce.span.error(None)
                    )
                }
                CodegenErrorKind::MissingArgumentDefinition(ad) => {
                    write!(f, "\nError: Missing Argument Definition For \"{}\"\n{}", ad, ce.span.error(None))
                }
                CodegenErrorKind::MissingMacroDefinition(md) => {
                    write!(f, "\nError: Missing Macro Definition For \"{}\"\n{}", md, ce.span.file())
                }
                CodegenErrorKind::InvalidMacroInvocation(mmi) => {
                    write!(f, "\nError: Missing Macro Definition For Invocation: \"{}\"\n{}\n", mmi, ce.span.error(None))
                }
                CodegenErrorKind::MissingFunctionInterface(func) => {
                    write!(f, "\nError: Missing Function Interface: \"{}\"\n{}\n", func, ce.span.error(None))
                }
                CodegenErrorKind::MissingEventInterface(event) => {
                    write!(f, "\nError: Missing Event Interface: \"{}\"\n{}\n", event, ce.span.error(None))
                }
                CodegenErrorKind::MissingConstantDefinition(_) => {
                    write!(f, "\nError: Missing Constant Definition\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::MissingErrorDefinition(_) => {
                    write!(f, "\nError: Missing Error Definition\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::AbiGenerationFailure => {
                    write!(f, "\nError: ABI Generation Failed\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::IOError(ioe) => {
                    write!(f, "\nError: IO Error: {ioe}\n{}", ce.span.file())
                }
                CodegenErrorKind::UnkownArgcallType => {
                    write!(f, "\nError: Unknown Arg Call Type\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::MissingMacroInvocation(mmi) => {
                    write!(f, "\nError: Missing Macro Invocation: \"{}\"\n{}\n", mmi, ce.span.error(None))
                }
                CodegenErrorKind::UnmatchedJumpLabels(labels) => {
                    write!(f, "\nError: Unmatched Jump Labels: \"{}\"\n{}\n", labels.join(", "), ce.span.error(None))
                }
                CodegenErrorKind::UsizeConversion(_) => {
                    write!(f, "\nError: Usize Conversion\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::InvalidArguments(_) => {
                    write!(f, "\nError: Invalid Arguments\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::InvalidHex(_) => {
                    write!(f, "\nError: Invalid Hex\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::InvalidTableStatement(_) => {
                    write!(f, "\nError: Invalid Table Statement\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::InvalidCodeLength(_) => {
                    write!(f, "\nError: Invalid Code Length\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::TestInvocation(_) => {
                    write!(f, "\nError: Test Invocation\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::InvalidDynArgIndex => {
                    write!(f, "\nError: Invalid Dynamic Constructor Argument Index:\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::MissingTableSize(table_name) => {
                    write!(f, "\nError: Missing Table Size for table: \"{table_name}\"\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::UnsupportedBuiltinFunction(bf) => {
                    write!(f, "\nError: Unsupported Builtin Function: \"{bf}\"\n{}\n", ce.span.error(None))
                }
                CodegenErrorKind::UnsupportedStatementType(st) => {
                    write!(f, "\nError: Unsupported Statement Type: \"{st}\"\n{}\n", ce.span.error(None))
                }
            },
            CompilerError::FailedCompiles(v) => {
                v.iter().for_each(|ce| {
                    let _ = write!(f, "{ce}");
                });
                Ok(())
            }
            CompilerError::EmptyImportPath(os_str) => {
                write!(f, "\nError: Import path with empty string: \"{}\"", os_str.as_os_str().to_str().unwrap_or("<unknown import>"))
            }
        }
    }
}
