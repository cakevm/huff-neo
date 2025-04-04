use huff_neo_codegen::Codegen;
use huff_neo_utils::ast::span::AstSpan;
use huff_neo_utils::prelude::*;
use indexmap::IndexMap;
use std::collections::HashSet;
use std::{
    collections::BTreeMap,
    sync::{Arc, Mutex},
};

#[test]
fn constructs_valid_abi() {
    let constructor = MacroDefinition {
        name: "CONSTRUCTOR".to_string(),
        decorator: None,
        parameters: vec![],
        statements: vec![],
        takes: 0,
        returns: 0,
        span: AstSpan(vec![]),
        outlined: false,
        test: false,
    };
    let contract = Contract {
        macros: IndexMap::from([("CONSTRUCTOR".to_string(), constructor)]),
        invocations: vec![],
        imports: vec![],
        constants: Arc::new(Mutex::new(vec![])),
        errors: vec![],
        functions: vec![],
        events: vec![],
        tables: vec![],
        labels: HashSet::new(),
    };

    // Generate the abi from the contract
    let mut cg = Codegen::new();
    let abi = cg.abi_gen(contract, None).unwrap();
    assert_eq!(
        abi,
        Abi {
            constructor: Some(Constructor { inputs: vec![] }),
            functions: BTreeMap::new(),
            events: BTreeMap::new(),
            errors: BTreeMap::new(),
            receive: false,
            fallback: false
        }
    );
}

#[test]
fn missing_constructor_fails() {
    let _constructor = MacroDefinition {
        name: "CONSTRUCTOR".to_string(),
        decorator: None,
        parameters: vec![],
        statements: vec![],
        takes: 0,
        returns: 0,
        span: AstSpan(vec![]),
        outlined: false,
        test: false,
    };
    let contract = Contract {
        macros: IndexMap::new(),
        invocations: vec![],
        imports: vec![],
        constants: Arc::new(Mutex::new(vec![])),
        errors: vec![],
        functions: vec![],
        events: vec![],
        tables: vec![],
        labels: HashSet::new(),
    };

    // Generate the abi from the contract
    // This should fail since there's no constructor
    let mut cg = Codegen::new();
    let abi = cg.abi_gen(contract, None);
    assert!(abi.unwrap().constructor.is_none());
}
