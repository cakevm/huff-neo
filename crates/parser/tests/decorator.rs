use huff_neo_lexer::Lexer;
use huff_neo_parser::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::prelude::*;

/// Lexes and parses the source into a contract.
fn parse(source: &str) -> Result<Contract, ParserError> {
    let flattened_source = FullFileSource { source, file: None, spans: vec![] };
    let tokens = Lexer::new(flattened_source).map(|x| x.unwrap()).collect::<Vec<Token>>();
    Parser::new(tokens, None).parse()
}

/// Parses the source and returns the decorator flags of the named macro.
fn parse_flags(source: &str, name: &str) -> Vec<DecoratorFlag> {
    let mut contract = parse(source).unwrap();
    contract.macros.swap_remove(name).unwrap().decorator.unwrap().flags
}

/// Parses the source and returns the parser error.
fn parse_error(source: &str) -> ParserError {
    parse(source).unwrap_err()
}

/// A decorator flag without spans, for comparing parse results
#[derive(Debug, PartialEq)]
struct FlagShape {
    name: String,
    args: Vec<ArgShape>,
}

/// A decorator flag argument without its span
#[derive(Debug, PartialEq)]
struct ArgShape {
    key: Option<String>,
    value: DecoratorValue,
}

impl FlagShape {
    /// A flag with positional arguments only
    fn positional(name: &str, values: Vec<DecoratorValue>) -> Self {
        Self { name: name.to_string(), args: values.into_iter().map(|value| ArgShape { key: None, value }).collect() }
    }
}

/// The parsed flags without spans
fn shape(flags: &[DecoratorFlag]) -> Vec<FlagShape> {
    flags
        .iter()
        .map(|f| FlagShape {
            name: f.name.clone(),
            args: f.args.iter().map(|a| ArgShape { key: a.key.clone(), value: a.value.clone() }).collect(),
        })
        .collect()
}

#[test]
fn stacked_blocks_equal_one_block() {
    let single = r#"
    #[caller(0xbeef), timestamp(1700000000), expect_revert,]
    #define test T() = {}
    "#;
    let stacked = r#"
    #[caller(0xbeef)]
    #[timestamp(1700000000),]
    #[expect_revert]
    #define test T() = {}
    "#;

    let single_flags = parse_flags(single, "T");
    assert_eq!(shape(&single_flags), shape(&parse_flags(stacked, "T")));
    assert_eq!(
        shape(&single_flags),
        vec![
            FlagShape::positional("caller", vec![DecoratorValue::Hex("beef".to_string())]),
            FlagShape::positional("timestamp", vec![DecoratorValue::Int(1700000000)]),
            FlagShape::positional("expect_revert", vec![]),
        ]
    );
}

#[test]
fn parses_every_argument_kind() {
    let source = r#"
    #define error Unauthorized()
    #define constant SLOT = 0x02
    #[flag(0x0001, 42, "0x1234", true, false, [SLOT], this, at = 0x1000, __ERROR(Unauthorized), __RIGHTPAD(__FUNC_SIG("transfer(address,uint256)")),)]
    #define test T() = {}
    "#;

    let flags = parse_flags(source, "T");
    assert_eq!(flags.len(), 1);
    let values: Vec<_> = flags[0].args.iter().map(|a| (a.key.clone(), a.value.clone())).collect();

    assert_eq!(values[0], (None, DecoratorValue::Hex("0001".to_string())), "hex literal keeps its width");
    assert_eq!(values[1], (None, DecoratorValue::Int(42)));
    assert_eq!(values[2], (None, DecoratorValue::Str("0x1234".to_string())));
    assert_eq!(values[3], (None, DecoratorValue::Bool(true)));
    assert_eq!(values[4], (None, DecoratorValue::Bool(false)));
    assert_eq!(values[5], (None, DecoratorValue::Constant("SLOT".to_string())));
    assert_eq!(values[6], (None, DecoratorValue::Ident("this".to_string())));
    assert_eq!(values[7], (Some("at".to_string()), DecoratorValue::Hex("1000".to_string())));

    let DecoratorValue::Builtin(error) = &values[8].1 else { panic!("expected a builtin call, got {:?}", values[8].1) };
    assert_eq!(error.kind, BuiltinFunctionKind::Error);
    assert!(matches!(&error.args[..], [BuiltinFunctionArg::Identifier(name, _)] if name == "Unauthorized"));

    let DecoratorValue::Builtin(pad) = &values[9].1 else { panic!("expected a builtin call, got {:?}", values[9].1) };
    assert_eq!(pad.kind, BuiltinFunctionKind::RightPad);
    let [BuiltinFunctionArg::BuiltinFunctionCall(sig)] = &pad.args[..] else { panic!("expected a nested builtin, got {:?}", pad.args) };
    assert_eq!(sig.kind, BuiltinFunctionKind::FunctionSignature);
}

#[test]
fn keywords_are_plain_identifiers_inside_decorators() {
    let source = r#"
    #[view(pure, macro, takes, calldata)]
    #define test T() = {}
    "#;

    let flags = parse_flags(source, "T");
    let values: Vec<_> = flags[0].args.iter().map(|a| a.value.clone()).collect();
    assert_eq!(flags[0].name, "view");
    assert_eq!(values, ["pure", "macro", "takes", "calldata"].iter().map(|s| DecoratorValue::Ident(s.to_string())).collect::<Vec<_>>());
}

#[test]
fn decorator_after_constant_and_abi_definitions() {
    let source = r#"
    #define function transfer(address,uint256) nonpayable returns ()
    #[calldata(__FUNC_SIG(transfer))]
    #define test A() = {}
    #define constant K = 0x01
    #[store([K], 0x02)]
    #define test B() = {}
    "#;

    assert_eq!(parse_flags(source, "A")[0].name, "calldata");
    assert_eq!(
        shape(&parse_flags(source, "B")),
        vec![FlagShape::positional("store", vec![DecoratorValue::Constant("K".to_string()), DecoratorValue::Hex("02".to_string())])]
    );
}

#[test]
fn decorator_on_non_test_macro_fails() {
    for kind in ["macro", "fn"] {
        let err = parse_error(&format!("#[value(0x01)]\n#define {kind} M() = {{}}"));
        assert_eq!(err.kind, ParserErrorKind::DecoratorOnNonTest("M".to_string()));
        assert_eq!(err.spans, AstSpan(vec![Span::new(0..14, None)]), "the error points at the whole decorator");
    }
}

#[test]
fn flag_spans_point_at_the_name() {
    let source = "#[calldata(\"0x01\"), store([K], 0x02)] #define test T() = {}";

    let flags = parse_flags(source, "T");
    assert_eq!(flags[0].span, AstSpan(vec![Span::new(2..10, None)]));
    assert_eq!(flags[1].span, AstSpan(vec![Span::new(20..25, None)]));
    assert_eq!(flags[1].args[0].span, AstSpan(vec![Span::new(26..29, None)]), "a constant argument spans its brackets");
}

#[test]
fn flags_without_commas_still_parse() {
    let with_commas = parse_flags("#[caller(0x01), expect_revert, value(0x01)] #define test T() = {}", "T");
    let without_commas = parse_flags("#[caller(0x01) expect_revert value(0x01)] #define test T() = {}", "T");
    assert_eq!(shape(&with_commas), shape(&without_commas));
}

#[test]
fn non_flag_token_between_flags_fails() {
    let err = parse_error("#[caller(0x01) 0x02] #define test T() = {}");
    assert!(matches!(err.kind, ParserErrorKind::InvalidDecoratorFlag(_)), "{err:?}");
}

#[test]
fn missing_comma_between_args_fails() {
    let err = parse_error("#[store(0x01 0x02)] #define test T() = {}");
    assert!(matches!(err.kind, ParserErrorKind::InvalidDecoratorFlag(_)), "{err:?}");
}

#[test]
fn invalid_argument_token_fails() {
    let err = parse_error("#[caller(:)] #define test T() = {}");
    assert!(matches!(err.kind, ParserErrorKind::InvalidDecoratorFlagArg(TokenKind::Colon)), "{err:?}");
}

#[test]
fn flag_name_must_be_an_identifier() {
    let err = parse_error("#[0x01] #define test T() = {}");
    assert!(matches!(err.kind, ParserErrorKind::InvalidDecoratorFlag(_)), "{err:?}");
}
