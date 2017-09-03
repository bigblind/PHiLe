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
    let expected_ast = Prog {
        items: vec![
            Item::StructDecl(StructDecl {
                name: "Foo",
                fields: vec![],
                range: Range {
                    begin: Location { src_idx: 0, line: 1, column:  1 },
                    end:   Location { src_idx: 0, line: 1, column: 14 },
                },
            }),
            Item::ClassDecl(ClassDecl {
                name: "Bar",
                fields: vec![],
                range: Range {
                    begin: Location { src_idx: 0, line: 1, column: 15 },
                    end:   Location { src_idx: 0, line: 1, column: 27 },
                },
            }),
            Item::StructDecl(StructDecl {
                name: "Single",
                fields: vec![
                    Field {
                        range: Range {
                            begin: Location { src_idx: 1, line: 1, column: 17 },
                            end:   Location { src_idx: 1, line: 1, column: 34 },
                        },
                        name: "some_field",
                        ty: Ty {
                            kind: TyKind::Optional(
                                Box::new(Ty {
                                    kind: TyKind::Named("int"),
                                    range: Range {
                                        begin: Location { src_idx: 1, line: 1, column: 29 },
                                        end:   Location { src_idx: 1, line: 1, column: 32 },
                                    },
                                })
                            ),
                            range: Range {
                                begin: Location { src_idx: 1, line: 1, column: 29 },
                                end:   Location { src_idx: 1, line: 1, column: 33 },
                            },
                        },
                        relation: None,
                    },
                ],
                range: Range {
                    begin: Location { src_idx: 1, line: 1, column:  1 },
                    end:   Location { src_idx: 1, line: 1, column: 36 },
                },
            }),
            Item::ClassDecl(ClassDecl {
                name: "One",
                fields: vec![
                    Field {
                        range: Range {
                            begin: Location { src_idx: 1, line: 1, column: 49 },
                            end:   Location { src_idx: 1, line: 1, column: 63 },
                        },
                        name: "name",
                        ty: Ty {
                            kind: TyKind::Array(
                                Box::new(Ty {
                                    kind: TyKind::Pointer(
                                        Box::new(Ty {
                                            kind: TyKind::Named("Type"),
                                            range: Range {
                                                begin: Location { src_idx: 1, line: 1, column: 57 },
                                                end:   Location { src_idx: 1, line: 1, column: 61 },
                                            },
                                        })
                                    ),
                                    range: Range {
                                        begin: Location { src_idx: 1, line: 1, column: 56 },
                                        end:   Location { src_idx: 1, line: 1, column: 61 },
                                    },
                                })
                            ),
                            range: Range {
                                begin: Location { src_idx: 1, line: 1, column: 55 },
                                end:   Location { src_idx: 1, line: 1, column: 62 },
                            },
                        },
                        relation: None,
                    },
                ],
                range: Range {
                    begin: Location { src_idx: 1, line: 1, column: 37 },
                    end:   Location { src_idx: 1, line: 1, column: 65 },
                },
            }),
        ]
    };

    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    assert_eq!(actual_ast, expected_ast);
}
