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
#[macro_use]
extern crate itertools;
extern crate regex;
extern crate phile;

use regex::Regex;
use phile::lexer::{ self, Token, TokenKind, Location, Range };
use phile::ast::*;
use phile::error::*;
use phile::parser;
use phile::util::grapheme_count;


#[derive(Debug)]
struct InvalidTestCase {
    source:  &'static str,
    marker:  &'static str,
    message: &'static str,
}

fn lex_filter_ws_comment<'a, S: AsRef<str>>(sources: &'a [S]) -> Vec<Token<'a>> {
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

// Helpers for `valid_fn_def()`.

// Calls the function `f` so that it can append to the end
// of the buffer `buf`. Returns the return value of the function
// and the range of the suffix which has been appended onto `buf`.
// Assumes that `buf` is a single line (no newlines allowed).
// The `src_idx`es of the locations of the returned range are 0.
// TODO(H2CO3): this is linear in the final length of `buf`,
// because it uses `grapheme_count()` to compute the range.
// This should not be a problem as long as it's being called
// infrequently (i.e. a low, constant number of times).
fn range_by_appending_with<T, F>(buf: &mut String, f: F) -> (T, Range)
    where F: FnOnce(&mut String) -> T {

    let start = 1 + grapheme_count(buf);
    let value = f(buf);
    let end = 1 + grapheme_count(buf);
    let range = oneline_range(0, start..end);
    (value, range)
}

// Appends the string `s` to the end of the buffer `buf`,
// and returns the range of this new suffix.
// Assumes that `buf` is a single line (no newlines allowed).
// The `src_idx`es of the locations of the returned range are 0.
fn range_by_appending(buf: &mut String, s: &str) -> Range {
    let (_, range) = range_by_appending_with(buf, |buf| *buf += s);
    range
}

// Helper for `valid_fn_def()`.
fn valid_fn_source_and_ast(
    (
        args,
        fn_name,
        ret_type_str,
        body_str,
        has_arg_type,
        has_ret_type,
        has_body,
        trailing_comma,
    ): (
        &[(&'static str, &'static str)],
        &'static str,
        &'static str,
        &'static str,
        bool,
        bool,
        bool,
        bool,
    )
) -> (String, Function<'static>) {
    let mut buf = String::new();

    let ((arguments, ret_type, body), range) = range_by_appending_with(&mut buf, |buf| {
        let mut arguments = Vec::with_capacity(args.len());

        *buf += "fn ";
        *buf += fn_name;
        *buf += "(";

        for i in 0..args.len() {
            let (arg_name, arg_type_name) = args[i];

            let (arg_type, arg_range) = range_by_appending_with(buf, |buf| {
                *buf += arg_name;

                if has_arg_type {
                    *buf += ": ";

                    Some(Ty {
                        range: range_by_appending(buf, arg_type_name),
                        kind: TyKind::Named(arg_type_name),
                    })
                } else {
                    None
                }
            });

            arguments.push(FuncArg {
                range: arg_range,
                name: arg_name,
                ty: arg_type,
            });

            if trailing_comma || i < args.len() - 1 {
                *buf += ", ";
            }
        }

        *buf += ")";

        let ret_type = if has_ret_type {
            *buf += " -> ";

            Some(Ty {
                range: range_by_appending(buf, ret_type_str),
                kind: TyKind::Named(ret_type_str),
            })
        } else {
            None
        };

        let body = if has_body {
            let (inner_range, block_range) = range_by_appending_with(buf, |buf| {
                *buf += "{";
                let inner_range = range_by_appending(buf, body_str);
                *buf += "}";
                inner_range
            });

            Exp {
                range: block_range,
                kind: ExpKind::Block(vec![
                    Exp {
                        range: inner_range,
                        kind: ExpKind::Identifier(body_str),
                    },
                ]),
            }
        } else {
            Exp {
                range: range_by_appending(buf, "{}"),
                kind: ExpKind::Block(vec![]),
            }
        };

        (arguments, ret_type, body)
    });

    let name = Some(fn_name);
    let func = Function { range, name, arguments, ret_type, body };

    (buf, func)
}

#[test]
fn valid_fn_def() {
    let args_name_type = [("foo", "String"), ("bar", "float")];
    let args = (0..args_name_type.len() + 1).map(|n| &args_name_type[..n]);
    let fn_names = vec!["some_func", "_", "noname", "nonIdiomaticFunction"];
    let ret_type_strs = vec!["Quxy", "weirdType"];
    let body_strs = vec!["the_value", "AnotherValue", "_"];
    let arg_flags   = vec![false, true];
    let ret_flags   = vec![false, true];
    let body_flags  = vec![false, true];
    let comma_flags = vec![false, true];

    let it = iproduct!(args, fn_names, ret_type_strs, body_strs, arg_flags, comma_flags, ret_flags, body_flags);

    for params in it {
        let (buf, func) = valid_fn_source_and_ast(params);
        let sources = [buf];
        let tokens = lex_filter_ws_comment(&sources);
        let actual_ast = parse_valid(&tokens);
        let expected_ast = Prog { items: vec![Item::FuncDef(func)] };

        assert_eq!(actual_ast, expected_ast);
    }
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
    // Wrap up every expression in the body of a minimal function.
    // `make_range()` is a small helper that translates the ranges
    // as visually perceived by inspection of the `exprs` array
    // to the actual ranges in the AST. These will just be shifted
    // by the length of the header of the wrapping fn, `prefix`.
    let prefix = "fn _() { ";
    let suffix = " }";
    let make_range = |i, r: std::ops::Range<usize>| {
        let shift = grapheme_count(prefix);
        oneline_range(i, r.start + shift..r.end + shift)
    };

    // Expressions to be parsed and the corresponding expected AST
    // node fragments. Each node fragment will be the only element
    // in the body block of one of the array of wrapping functions.
    let cases = vec![
        // Atomic expressions
        (
            "nil",
            Exp {
                kind: ExpKind::NilLiteral,
                range: make_range(0, 1..4),
            },
        ),
        (
            "true",
            Exp {
                kind: ExpKind::BoolLiteral("true"),
                range: make_range(1, 1..5),
            },
        ),
        (
            "false",
            Exp {
                kind: ExpKind::BoolLiteral("false"),
                range: make_range(2, 1..6),
            },
        ),
        (
            "42",
            Exp {
                kind: ExpKind::IntLiteral("42"),
                range: make_range(3, 1..3),
            },
        ),
        (
            "13.37",
            Exp {
                kind: ExpKind::FloatLiteral("13.37"),
                range: make_range(4, 1..6),
            },
        ),
        (
            r#""""#, // empty string
            Exp {
                kind: ExpKind::StringLiteral(r#""""#),
                range: make_range(5, 1..3),
            },
        ),
        (
            r#""not empty""#, // non-empty string
            Exp {
                kind: ExpKind::StringLiteral(r#""not empty""#),
                range: make_range(6, 1..12),
            },
        ),
        (
            r#""\"\\""#, // \"\\ (escaped escape char and quote)
            Exp {
                kind: ExpKind::StringLiteral(r#""\"\\""#),
                range: make_range(7, 1..7),
            },
        ),
        (
            "\"\u{6F22}\"", // Unicode (CJK) character
            Exp {
                kind: ExpKind::StringLiteral("\"\u{6F22}\""),
                range: make_range(8, 1..4),
            },
        ),
        (
            r#""\u{6F22}""#, // Unicode (CJK) character, escaped
            Exp {
                kind: ExpKind::StringLiteral(r#""\u{6F22}""#),
                range: make_range(9, 1..11),
            },
        ),
        (
            "_", // identifier
            Exp {
                kind: ExpKind::Identifier("_"),
                range: make_range(10, 1..2),
            },
        ),
        (
            "lower_snake_case",
            Exp {
                kind: ExpKind::Identifier("lower_snake_case"),
                range: make_range(11, 1..17),
            },
        ),
        (
            "UPPER_SNAKE_CASE",
            Exp {
                kind: ExpKind::Identifier("UPPER_SNAKE_CASE"),
                range: make_range(12, 1..17),
            },
        ),
        (
            "lowerCamelCase",
            Exp {
                kind: ExpKind::Identifier("lowerCamelCase"),
                range: make_range(13, 1..15),
            },
        ),
        (
            "UpperCamelCase",
            Exp {
                kind: ExpKind::Identifier("UpperCamelCase"),
                range: make_range(14, 1..15),
            },
        ),

        // Non-atomic terms
        (
            "()",
            Exp {
                kind: ExpKind::TupleLiteral(vec![]),
                range: make_range(15, 1..3),
            },
        ),
        (
            "(())",
            Exp {
                kind: ExpKind::TupleLiteral(vec![
                    Exp {
                        kind: ExpKind::TupleLiteral(vec![]),
                        range: make_range(16, 2..4),
                    },
                ]),
                range: make_range(16, 1..5),
            },
        ),
        (
            "((),())",
            Exp {
                kind: ExpKind::TupleLiteral(vec![
                    Exp {
                        kind: ExpKind::TupleLiteral(vec![]),
                        range: make_range(17, 2..4),
                    },
                    Exp {
                        kind: ExpKind::TupleLiteral(vec![]),
                        range: make_range(17, 5..7),
                    },
                ]),
                range: make_range(17, 1..8),
            },
        ),
        (
            "(foo)",
            Exp {
                kind: ExpKind::TupleLiteral(vec![
                    Exp {
                        kind: ExpKind::Identifier("foo"),
                        range: make_range(18, 2..5),
                    },
                ]),
                range: make_range(18, 1..6),
            },
        ),
        (
            "((nil),)",
            Exp {
                kind: ExpKind::TupleLiteral(vec![
                    Exp {
                        kind: ExpKind::TupleLiteral(vec![
                            Exp {
                                kind: ExpKind::NilLiteral,
                                range: make_range(19, 3..6),
                            },
                        ]),
                        range: make_range(19, 2..7),
                    },
                ]),
                range: make_range(19, 1..9),
            },
        ),
        (
            "(1, 0, 0.0, 99.56)",
            Exp {
                kind: ExpKind::TupleLiteral(vec![
                    Exp {
                        kind: ExpKind::IntLiteral("1"),
                        range: make_range(20, 2..3),
                    },
                    Exp {
                        kind: ExpKind::IntLiteral("0"),
                        range: make_range(20, 5..6),
                    },
                    Exp {
                        kind: ExpKind::FloatLiteral("0.0"),
                        range: make_range(20, 8..11),
                    },
                    Exp {
                        kind: ExpKind::FloatLiteral("99.56"),
                        range: make_range(20, 13..18),
                    },
                ]),
                range: make_range(20, 1..19),
            },
        ),
        (
            "[]",
            Exp {
                kind: ExpKind::ArrayLiteral(vec![]),
                range: make_range(21, 1..3),
            },
        ),
        (
            "[[]]",
            Exp {
                kind: ExpKind::ArrayLiteral(vec![
                    Exp {
                        kind: ExpKind::ArrayLiteral(vec![]),
                        range: make_range(22, 2..4),
                    },
                ]),
                range: make_range(22, 1..5),
            },
        ),
        (
            "[[],[]]",
            Exp {
                kind: ExpKind::ArrayLiteral(vec![
                    Exp {
                        kind: ExpKind::ArrayLiteral(vec![]),
                        range: make_range(23, 2..4),
                    },
                    Exp {
                        kind: ExpKind::ArrayLiteral(vec![]),
                        range: make_range(23, 5..7),
                    },
                ]),
                range: make_range(23, 1..8),
            },
        ),
        (
            "[[true, false, ]]",
            Exp {
                kind: ExpKind::ArrayLiteral(vec![
                    Exp {
                        kind: ExpKind::ArrayLiteral(vec![
                            Exp {
                                kind: ExpKind::BoolLiteral("true"),
                                range: make_range(24, 3..7),
                            },
                            Exp {
                                kind: ExpKind::BoolLiteral("false"),
                                range: make_range(24, 9..14),
                            },
                        ]),
                        range: make_range(24, 2..17),
                    },
                ]),
                range: make_range(24, 1..18),
            },
        ),
        (
            "{}",
            Exp {
                kind: ExpKind::Block(vec![]),
                range: make_range(25, 1..3),
            },
        ),
        (
            "{{}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: make_range(26, 2..4),
                    },
                ]),
                range: make_range(26, 1..5),
            },
        ),
        (
            "{{}{}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: make_range(27, 2..4),
                    },
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: make_range(27, 4..6),
                    },
                ]),
                range: make_range(27, 1..7),
            },
        ),
        (
            "{{};{}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: make_range(28, 2..4),
                    },
                    Exp {
                        kind: ExpKind::Empty,
                        range: make_range(28, 4..5),
                    },
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: make_range(28, 5..7),
                    },
                ]),
                range: make_range(28, 1..8),
            },
        ),
        (
            "{3.1415927; {()}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Semi(
                            Box::new(Exp {
                                kind: ExpKind::FloatLiteral("3.1415927"),
                                range: make_range(29, 2..11),
                            })
                        ),
                        range: make_range(29, 2..11),
                    },
                    Exp {
                        kind: ExpKind::Block(vec![
                            Exp {
                                kind: ExpKind::TupleLiteral(vec![]),
                                range: make_range(29, 14..16),
                            },
                        ]),
                        range: make_range(29, 13..17),
                    },
                ]),
                range: make_range(29, 1..18),
            },
        ),
        (
            "{0;;}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Semi(
                            Box::new(Exp {
                                kind: ExpKind::IntLiteral("0"),
                                range: make_range(30, 2..3),
                            })
                        ),
                        range: make_range(30, 2..3),
                    },
                    Exp {
                        kind: ExpKind::Empty,
                        range: make_range(30, 4..5),
                    },
                ]),
                range: make_range(30, 1..6),
            },
        ),
        (
            "{ let var_name = stuff; }",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::VarDecl(
                            Box::new(VarDecl {
                                name: "var_name",
                                ty: None,
                                expr: Exp {
                                    kind: ExpKind::Identifier("stuff"),
                                    range: make_range(31, 18..23),
                                },
                            })
                        ),
                        range: make_range(31, 3..24),
                    },
                ]),
                range: make_range(31, 1..26),
            },
        ),
        (
            "{ let x: T = 999; }",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::VarDecl(
                            Box::new(VarDecl {
                                name: "x",
                                ty: Some(Ty {
                                    kind: TyKind::Named("T"),
                                    range: make_range(32, 10..11),
                                }),
                                expr: Exp {
                                    kind: ExpKind::IntLiteral("999"),
                                    range: make_range(32, 14..17),
                                },
                            })
                        ),
                        range: make_range(32, 3..18),
                    },
                ]),
                range: make_range(32, 1..20),
            },
        ),
    ];

    // The actual sources to be parsed are formed by concatenating
    // the header of the function (including the opening '{' of
    // its body block), the test case expression, and the closing
    // '}' of the body block.
    // Also form the actual top-level items, which are function
    // definitions, by using the expected AST fragments.
    // The body of the function is not the `node` expression
    // itself, but a block that contains `node` as its only
    // element. This top-level body block thus also has a
    // source range; that is what we are calculating below.
    let iter = cases.into_iter().enumerate().map(|(i, (src, node))| {
        let src = [prefix, src, suffix].join("");
        let start_byte_index = src.find('{').unwrap();
        let end_byte_index = src.rfind('}').unwrap() + 1;
        let start = 1 + grapheme_count(&src[..start_byte_index]);
        let end = 1 + grapheme_count(&src[..end_byte_index]);
        let func_range = oneline_range(i, 1..end);
        let exp_range = oneline_range(i, start..end);

        let item = Item::FuncDef(Function {
            range: func_range,
            name: Some("_"),
            arguments: vec![],
            ret_type: None,
            body: Exp {
                kind: ExpKind::Block(vec![node]),
                range: exp_range,
            },
        });

        (src, item)
    });

    // Finally, parse the array of sources, and compare the resulting
    // (actual) items to the expected ones constructed above.
    let (sources, items): (Vec<_>, Vec<_>) = iter.unzip();
    let expected_ast = Prog { items };
    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    assert_eq!(actual_ast, expected_ast);
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
