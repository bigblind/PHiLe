//
// tests/parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 27/08/2017
//

#![cfg(test)]
#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications)]

#[macro_use]
extern crate quickcheck;
extern crate phile;

use phile::lexer::{ self, Token, TokenKind, Location, Range };
use phile::ast;
use phile::parser;


fn lex_filter_ws_comment<'a>(sources: &'a [&str]) -> Vec<Token<'a>> {
    let mut tokens = lexer::lex(sources).unwrap();

    tokens.retain(|token| match token.kind {
        TokenKind::Whitespace => false,
        TokenKind::Comment => false,
        _ => true,
    });

    tokens
}

fn parse_valid<'a>(tokens: &'a [Token]) -> ast::Prog<'a> {
    parser::parse(tokens).unwrap()
}

#[test]
fn empty_source() {
    match parser::parse(&[]) {
        Ok(ast) => assert!(ast.is_empty()),
        Err(err) => panic!("Empty source erroneously rejected: {}", err),
    }
}

#[test]
fn empty_struct_or_class_decl() {
    let sources = ["struct Foo {} class Bar {}"];
    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    let expected_ast = vec![
        ast::Item::StructDecl(ast::StructDecl {
            name: "Foo",
            fields: vec![],
            range: Range {
                begin: Location {
                    src_idx: 0,
                    line: 1,
                    column: 1,
                },
                end: Location {
                    src_idx: 0,
                    line: 1,
                    column: 14,
                },
            },
        }),
        ast::Item::ClassDecl(ast::ClassDecl {
            name: "Bar",
            fields: vec![],
            range: Range {
                begin: Location {
                    src_idx: 0,
                    line: 1,
                    column: 15,
                },
                end: Location {
                    src_idx: 0,
                    line: 1,
                    column: 27,
                },
            },
        }),
    ];

    assert_eq!(actual_ast, expected_ast);
}
