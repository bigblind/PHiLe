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
extern crate lazy_static;
extern crate regex;
extern crate phile;

use regex::Regex;
use phile::lexer::{ self, Token, TokenKind, Location, Range };
use phile::ast::*;
use phile::error::*;
use phile::parser;


#[derive(Debug)]
struct InvalidTestCase {
    source:  &'static str,
    marker:  &'static str,
    message: &'static str,
}

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
    parser::parse(tokens).expect("valid source was rejected")
}

fn parse_invalid(source: &str) -> (String, Range) {
    let sources = [source];
    let tokens = lex_filter_ws_comment(&sources);

    match parser::parse(&tokens) {
        Ok(_) => panic!("invalid source was accepted"),
        Err(Error::Syntax { message, range }) => (message, range),
        Err(err) => panic!("Parser returned a non-syntactic error: {}", err),
    }
}

fn oneline_range(src_idx: usize, char_range: std::ops::Range<usize>) -> Range {
    Range {
        start: Location { src_idx, line: 1, column: char_range.start },
        end:   Location { src_idx, line: 1, column: char_range.end   },
    }
}

#[allow(non_upper_case_globals)]
fn error_marker_range(marker: &str) -> Range {
    lazy_static! {
        static ref regex: Regex = Regex::new(r"^ *(\^_*\^) *$").unwrap();
    }

    let m = regex.captures(marker).unwrap().get(1).unwrap();
    let start_index = 1 + m.start();
    let end_index = 1 + m.end() - 1;

    oneline_range(0, start_index..end_index)
}

fn test_invalid_cases(test_cases: &[InvalidTestCase]) {
    for &InvalidTestCase { source, marker, message } in test_cases {
        let (actual_message, actual_range) = parse_invalid(source);
        let expected_range = error_marker_range(marker);
        let expected_message = message;

        assert_eq!(actual_message, expected_message);
        assert_eq!(actual_range, expected_range);
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
        "struct Multi { f0: (Tup, Le), f1: String -> float, }",
        // multiple fields with relations
        "class More { relative: &Qux? +<->? preimage, maybe_parent: &More *<->, }",
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
        Item::StructDecl(StructDecl {
            name: "Multi",
            fields: vec![
                Field {
                    range: oneline_range(2, 16..30),
                    name: "f0",
                    ty: Ty {
                        kind: TyKind::Tuple(vec![
                            Ty {
                                kind: TyKind::Named("Tup"),
                                range: oneline_range(2, 21..24),
                            },
                            Ty {
                                kind: TyKind::Named("Le"),
                                range: oneline_range(2, 26..28),
                            },
                        ]),
                        range: oneline_range(2, 20..29),
                    },
                    relation: None,
                },
                Field {
                    range: oneline_range(2, 31..51),
                    name: "f1",
                    ty: Ty {
                        kind: TyKind::Function(FunctionTy {
                            arg_types: vec![
                                Ty {
                                    kind: TyKind::Named("String"),
                                    range: oneline_range(2, 35..41),
                                },
                            ],
                            ret_type: Box::new(Ty {
                                kind: TyKind::Named("float"),
                                range: oneline_range(2, 45..50),
                            }),
                        }),
                        range: oneline_range(2, 35..50),
                    },
                    relation: None,
                },
            ],
            range: oneline_range(2, 1..53),
        }),
        Item::ClassDecl(ClassDecl {
            name: "More",
            fields: vec![
                Field {
                    range: oneline_range(3, 14..45),
                    name: "relative",
                    ty: Ty {
                        kind: TyKind::Optional(
                            Box::new(Ty {
                                kind: TyKind::Pointer(
                                    Box::new(Ty {
                                        kind: TyKind::Named("Qux"),
                                        range: oneline_range(3, 25..28),
                                    })
                                ),
                                range: oneline_range(3, 24..28),
                            })
                        ),
                        range: oneline_range(3, 24..29),
                    },
                    relation: Some(RelDecl {
                        cardinality: "+<->?",
                        field: Some("preimage"),
                    }),
                },
                Field {
                    range: oneline_range(3, 46..71),
                    name: "maybe_parent",
                    ty: Ty {
                        kind: TyKind::Pointer(
                            Box::new(Ty {
                                kind: TyKind::Named("More"),
                                range: oneline_range(3, 61..65),
                            })
                        ),
                        range: oneline_range(3, 60..65),
                    },
                    relation: Some(RelDecl {
                        cardinality: "*<->",
                        field: None,
                    }),
                },
            ],
            range: oneline_range(3, 1..73),
        }),
    ];

    let expected_ast = Prog { items };
    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    assert_eq!(actual_ast, expected_ast);
}

#[test]
fn invalid_struct_or_class_decl() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "struct",
            marker:  "^_____^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "class",
            marker:  "^____^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "struct if",
            marker:  "       ^_^",
            message: "Expected identifier; found if",
        },
        InvalidTestCase {
            source:  "class match",
            marker:  "      ^____^",
            message: "Expected identifier; found match",
        },
        InvalidTestCase {
            source:  "struct MyStruct",
            marker:  "       ^_______^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  "class Classy",
            marker:  "      ^_____^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  "struct Whatever {",
            marker:  "                ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  "class Error {",
            marker:  "            ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  "struct Struct { field ",
            marker:  "                ^____^",
            message: "Expected :; found end of input",
        },
        InvalidTestCase {
            source:  "class Struct { field ",
            marker:  "               ^____^",
            message: "Expected :; found end of input",
        },
        InvalidTestCase {
            source:  "struct Class { field: ",
            marker:  "                    ^^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  "class Class { field: ",
            marker:  "                   ^^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  "struct NoComma { field: int? }",
            marker:  "                             ^^",
            message: "Expected ,; found }",
        },
        InvalidTestCase {
            source:  "class Chameleon { field: [Bogus] }",
            marker:  "                                 ^^",
            message: "Expected ,; found }",
        },
        InvalidTestCase {
            source:  "struct NoCurly { name: String??,",
            marker:  "                               ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  "class NoBrace { field_name: &Loller, ",
            marker:  "                                   ^^",
            message: "Expected }; found end of input",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn valid_enum_decl() {
    let sources = [
        // empty, no variants
        "enum Never {}",
        // one variant without associated data
        "enum Unit { One, }",
        // one variant with associated data
        "enum NotQuiteUnit { Singleton(float?), }",
        // multiple variants without associated data
        "enum Bool { True, False, FileNotFound, }",
        // multiple variants with associated data
        "enum Goo { Lorem(String), Ipsum(()), }",
    ];

    let items = vec![
        Item::EnumDecl(EnumDecl {
            range: oneline_range(0, 1..14),
            name: "Never",
            variants: vec![],
        }),
        Item::EnumDecl(EnumDecl {
            range: oneline_range(1, 1..19),
            name: "Unit",
            variants: vec![
                Variant {
                    range: oneline_range(1, 13..17),
                    name: "One",
                    ty: None,
                },
            ],
        }),
        Item::EnumDecl(EnumDecl {
            range: oneline_range(2, 1..41),
            name: "NotQuiteUnit",
            variants: vec![
                Variant {
                    range: oneline_range(2, 21..39),
                    name: "Singleton",
                    ty: Some(Ty {
                        kind: TyKind::Optional(
                            Box::new(Ty {
                                kind: TyKind::Named("float"),
                                range: oneline_range(2, 31..36),
                            })
                        ),
                        range: oneline_range(2, 31..37),
                    }),
                },
            ],
        }),
        Item::EnumDecl(EnumDecl {
            range: oneline_range(3, 1..41),
            name: "Bool",
            variants: vec![
                Variant {
                    range: oneline_range(3, 13..18),
                    name: "True",
                    ty: None,
                },
                Variant {
                    range: oneline_range(3, 19..25),
                    name: "False",
                    ty: None,
                },
                Variant {
                    range: oneline_range(3, 26..39),
                    name: "FileNotFound",
                    ty: None,
                },
            ],
        }),
        Item::EnumDecl(EnumDecl {
            range: oneline_range(4, 1..39),
            name: "Goo",
            variants: vec![
                Variant {
                    range: oneline_range(4, 12..26),
                    name: "Lorem",
                    ty: Some(Ty {
                        kind: TyKind::Named("String"),
                        range: oneline_range(4, 18..24),
                    }),
                },
                Variant {
                    range: oneline_range(4, 27..37),
                    name: "Ipsum",
                    ty: Some(Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: oneline_range(4, 33..35),
                    }),
                },
            ],
        }),
    ];

    let expected_ast = Prog { items };
    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    assert_eq!(actual_ast, expected_ast);
}

#[test]
fn invalid_enum_decl() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "enum",
            marker:  "^___^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "enum true",
            marker:  "     ^___^",
            message: "Expected identifier; found true",
        },
        InvalidTestCase {
            source:  "enum Summy",
            marker:  "     ^____^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  "enum Foo {",
            marker:  "         ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  "enum Bar { SomeVariant ",
            marker:  "           ^__________^",
            message: "Expected ,; found end of input",
        },
        InvalidTestCase {
            source:  "enum Qux { Another } ",
            marker:  "                   ^^",
            message: "Expected ,; found }",
        },
        InvalidTestCase {
            source:  "enum Cassos1 { Data( ",
            marker:  "                   ^^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  "enum Cassos2 { Data(), }",
            marker:  "                    ^^",
            message: "Expected a type; found )",
        },
        InvalidTestCase {
            source:  "enum OnlyOneTypeAllowed { Data(int, Cassos3), }",
            marker:  "                                  ^^           ",
            message: "Expected ); found ,",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn valid_fn_def() {
}

#[test]
fn invalid_fn_def() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn",
            marker:  "^_^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  " fn class",
            marker:  "    ^____^",
            message: "Expected identifier; found class",
        },
        InvalidTestCase {
            source:  "fn reir { }",
            marker:  "        ^^ ",
            message: "Expected (; found {",
        },
        InvalidTestCase {
            source:  " fn foo_bar(",
            marker:  "           ^^",
            message: "Expected ); found end of input",
        },
        InvalidTestCase {
            source:  " fn boo_far(arg0",
            marker:  "            ^___^",
            message: "Expected , or ); found end of input",
        },
        InvalidTestCase {
            source:  " fn missing(qux:",
            marker:  "               ^^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  " fn typeless(qux:)",
            marker:  "                 ^^",
            message: "Expected a type; found )",
        },
        InvalidTestCase {
            source:  " fn bodyless(arrrg)",
            marker:  "                  ^^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  " fn body_not_block() true | false & true",
            marker:  "                     ^___^              ",
            message: "Expected {; found true",
        },
        InvalidTestCase {
            source:  " fn incomplete_body(wrong) {",
            marker:  "                           ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  " fn incomplete_body_2(nope) { 29 <= nope ",
            marker:  "                                    ^___^",
            message: "Expected ; or }; found end of input",
        },
        InvalidTestCase {
            source:  "fn typy(arg1: [bool], arg2: Bazoo) -> ",
            marker:  "                                   ^_^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  "fn no_body(some_param: &Loller) -> [String?]?",
            marker:  "                                            ^^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  "fn still_no_block_body() -> &Huzzah + 1",
            marker:  "                                    ^^ ",
            message: "Expected {; found +",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn valid_impl_def() {
    let sources = [
        // Empty impl
        "impl Empty {}",
        // Impl with one function
        "impl Singular { fn the_one() -> int { 42 } }",
        // Impl with multiple functions
        "impl Dual { fn x(x0: X) {} fn x_dagger(x0: XDagger) {} }",
    ];

    let items = vec![
        Item::Impl(Impl {
            range: oneline_range(0, 1..14),
            name: "Empty",
            functions: vec![],
        }),
        Item::Impl(Impl {
            range: oneline_range(1, 1..45),
            name: "Singular",
            functions: vec![
                Function {
                    range: oneline_range(1, 17..43),
                    name: Some("the_one"),
                    arguments: vec![],
                    ret_type: Some(Ty {
                        kind: TyKind::Named("int"),
                        range: oneline_range(1, 33..36),
                    }),
                    body: Exp {
                        range: oneline_range(1, 37..43),
                        kind: ExpKind::Block(vec![
                            Exp {
                                range: oneline_range(1, 39..41),
                                kind: ExpKind::IntLiteral("42"),
                            },
                        ]),
                    },
                },
            ],
        }),
        Item::Impl(Impl {
            range: oneline_range(2, 1..57),
            name: "Dual",
            functions: vec![
                Function {
                    range: oneline_range(2, 13..27),
                    name: Some("x"),
                    arguments: vec![
                        FuncArg {
                            range: oneline_range(2, 18..23),
                            name: "x0",
                            ty: Some(Ty {
                                kind: TyKind::Named("X"),
                                range: oneline_range(2, 22..23),
                            }),
                        },
                    ],
                    ret_type: None,
                    body: Exp {
                        range: oneline_range(2, 25..27),
                        kind: ExpKind::Block(vec![]),
                    }
                },
                Function {
                    range: oneline_range(2, 28..55),
                    name: Some("x_dagger"),
                    arguments: vec![
                        FuncArg {
                            range: oneline_range(2, 40..51),
                            name: "x0",
                            ty: Some(Ty {
                                kind: TyKind::Named("XDagger"),
                                range: oneline_range(2, 44..51),
                            }),
                        },
                    ],
                    ret_type: None,
                    body: Exp {
                        range: oneline_range(2, 53..55),
                        kind: ExpKind::Block(vec![]),
                    }
                },
            ],
        }),
    ];

    let expected_ast = Prog { items };
    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    assert_eq!(actual_ast, expected_ast);
}

#[test]
fn invalid_impl_def() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "impl",
            marker:  "^___^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "impl impl ",
            marker:  "     ^___^",
            message: "Expected identifier; found impl",
        },
        InvalidTestCase {
            source:  "impl nil",
            marker:  "     ^__^",
            message: "Expected identifier; found nil",
        },
        InvalidTestCase {
            source:  "impl Typy",
            marker:  "     ^___^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  "impl Imply { ",
            marker:  "           ^^",
            message: "Expected fn; found end of input",
        },
        InvalidTestCase {
            source:  "impl Funcy { fn",
            marker:  "             ^_^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "impl Funcy { fn }",
            marker:  "                ^^",
            message: "Expected identifier; found }",
        },
        InvalidTestCase {
            source:  "impl Recursive { impl Inner { } }",
            marker:  "                 ^___^           ",
            message: "Expected fn; found impl",
        },
        InvalidTestCase {
            source:  "impl NonFunction { struct Inner { } }",
            marker:  "                   ^_____^           ",
            message: "Expected fn; found struct",
        },
        InvalidTestCase {
            source:  "impl Almost { fn whoo(x: float) {}",
            marker:  "                                 ^^",
            message: "Expected fn; found end of input",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_toplevel() {
    // TODO(H2CO3): add more cases; could this be ~exhaustive using QuickCheck?
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "; ",
            marker:  "^^",
            message: "Expected struct, class, enum, fn or impl; found ;",
        },
        InvalidTestCase {
            source:  "   ... ...",
            marker:  "   ^__^   ",
            message: "Expected struct, class, enum, fn or impl; found ...",
        },
        InvalidTestCase {
            source:  "   ,   ",
            marker:  "   ^^  ",
            message: "Expected struct, class, enum, fn or impl; found ,",
        },
        InvalidTestCase {
            source:  "   ::   ",
            marker:  "   ^_^  ",
            message: "Expected struct, class, enum, fn or impl; found ::",
        },
        InvalidTestCase {
            source:  "~",
            marker:  "^^",
            message: "Expected struct, class, enum, fn or impl; found ~",
        },
        InvalidTestCase {
            source:  "  (       )",
            marker:  "  ^^       ",
            message: "Expected struct, class, enum, fn or impl; found (",
        },
        InvalidTestCase {
            source:  "]     [",
            marker:  "^^     ",
            message: "Expected struct, class, enum, fn or impl; found ]",
        },
        InvalidTestCase {
            source:  "   {  }",
            marker:  "   ^^  ",
            message: "Expected struct, class, enum, fn or impl; found {",
        },
        InvalidTestCase {
            source:  " >>  <<",
            marker:  " ^^    ",
            message: "Expected struct, class, enum, fn or impl; found >",
        },
        InvalidTestCase {
            source:  "!= !<->+ ",
            marker:  "^_^      ",
            message: "Expected struct, class, enum, fn or impl; found !=",
        },
        InvalidTestCase {
            source:  " 19 + 2.7182 * 314.15E-2",
            marker:  " ^_^                    ",
            message: "Expected struct, class, enum, fn or impl; found 19",
        },
        InvalidTestCase {
            source:  "    0.83 / (9.0 % 24)",
            marker:  "    ^___^            ",
            message: "Expected struct, class, enum, fn or impl; found 0.83",
        },
        InvalidTestCase {
            source:  "  if a + b < c { notATriangle } else { mayBeATriangle }",
            marker:  "  ^_^                                                  ",
            message: "Expected struct, class, enum, fn or impl; found if",
        },
        InvalidTestCase {
            source:  "match 0 { _ => _ }",
            marker:  "^____^            ",
            message: "Expected struct, class, enum, fn or impl; found match",
        },
        InvalidTestCase {
            source:  "else # this is not even valid *anywhere*...",
            marker:  "^___^",
            message: "Expected struct, class, enum, fn or impl; found else",
        },
        InvalidTestCase {
            source:  "as    [ Foo ?   ] #\t...nor is this...",
            marker:  "^_^                                   ",
            message: "Expected struct, class, enum, fn or impl; found as",
        },
        InvalidTestCase {
            source:  "  nil == 3   # ...or this one, for that matter",
            marker:  "  ^__^                                        ",
            message: "Expected struct, class, enum, fn or impl; found nil",
        },
        InvalidTestCase {
            source:  "  Nope # doesn't work",
            marker:  "  ^___^              ",
            message: "Expected struct, class, enum, fn or impl; found Nope",
        },
        InvalidTestCase {
            source:  r#"       "stringy stuff"   "#,
            marker:  r#"       ^______________^  "#,
            message: r#"Expected struct, class, enum, fn or impl; found "stringy stuff""#,
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn valid_expression() {
}

#[test]
fn invalid_expression() {
}

#[test]
fn valid_type() {
}

#[test]
fn invalid_type() {
}
