/// Tests lexing the Free Storage Pointer Keyword
use huff_neo_lexer::*;
use huff_neo_utils::file::full_file_source::FullFileSource;
use huff_neo_utils::{
    opcodes::{OPCODES, OPCODES_MAP},
    prelude::*,
};

#[test]
fn opcodes() {
    for opcode in OPCODES {
        let opcode = (*opcode).to_owned();
        let source = &format!(
            r#"
            #define macro TEST() = takes(0) returns(0) {}
                {opcode}
            {}
            "#,
            "{", "}",
        );
        let flattened_source = FullFileSource { source, file: None, spans: vec![] };
        let lexer = Lexer::new(flattened_source);

        let tokens = lexer.into_iter().map(|x| x.unwrap()).filter(|x| !matches!(x.kind, TokenKind::Whitespace)).collect::<Vec<Token>>();

        assert_eq!(tokens.get(tokens.len() - 3).unwrap().kind, TokenKind::Opcode(OPCODES_MAP.get(&opcode).unwrap().to_owned()),);
    }
}
