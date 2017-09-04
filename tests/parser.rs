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
use phile::ast::*;
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

fn parse_valid<'a>(tokens: &'a [Token]) -> Prog<'a> {
    parser::parse(tokens).unwrap()
}

fn oneline_range(src_idx: usize, char_range: std::ops::Range<usize>) -> Range {
    Range {
        begin: Location { src_idx, line: 1, column: char_range.start },
        end:   Location { src_idx, line: 1, column: char_range.end   },
    }
}

#[test]
fn empty_source() {
    match parser::parse(&[]) {
        Ok(ast) => assert!(ast.items.is_empty()),
        Err(err) => panic!("Empty source erroneously rejected: {}", err),
    }
}

#[test]
fn valid_struct_or_class_decl() {
    let sources = [
        // empty, no fields
        "struct Foo {} class Bar {}",
        // one field
        "struct Single { some_field: int?, } class One { name: [&Type], }",
        // multiple fields
        // "struct Multi { f0: (Tup, Le), f1: String -> float, }",
        // "class More { id: UUID, date_time: Date, }",
    ];
    let items = vec![
        Item::StructDecl(StructDecl {
            name: "Foo",
            fields: vec![],
            range: oneline_range(0, 1..14),
        }),
        Item::ClassDecl(ClassDecl {
            name: "Bar",
            fields: vec![],
            range: oneline_range(0, 15..27),
        }),
        Item::StructDecl(StructDecl {
            name: "Single",
            fields: vec![
                Field {
                    range: oneline_range(1, 17..34),
                    name: "some_field",
                    ty: Ty {
                        kind: TyKind::Optional(
                            Box::new(Ty {
                                kind: TyKind::Named("int"),
                                range: oneline_range(1, 29..32),
                            })
                        ),
                        range: oneline_range(1, 29..33),
                    },
                    relation: None,
                },
            ],
            range: oneline_range(1, 1..36),
        }),
        Item::ClassDecl(ClassDecl {
            name: "One",
            fields: vec![
                Field {
                    range: oneline_range(1, 49..63),
                    name: "name",
                    ty: Ty {
                        kind: TyKind::Array(
                            Box::new(Ty {
                                kind: TyKind::Pointer(
                                    Box::new(Ty {
                                        kind: TyKind::Named("Type"),
                                        range: oneline_range(1, 57..61),
                                    })
                                ),
                                range: oneline_range(1, 56..61),
                            })
                        ),
                        range: oneline_range(1, 55..62),
                    },
                    relation: None,
                },
            ],
            range: oneline_range(1, 37..65),
        }),
    ];

    let expected_ast = Prog { items };
    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    assert_eq!(actual_ast, expected_ast);
}
