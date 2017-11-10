//
// tests/parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 27/08/2017
//

#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications)]
#![cfg_attr(feature = "cargo-clippy",
            allow(match_same_arms, clone_on_ref_ptr))]
#![cfg_attr(feature = "cargo-clippy",
            deny(wrong_pub_self_convention, used_underscore_binding,
                 stutter, similar_names, pub_enum_variant_names,
                 non_ascii_literal, unicode_not_nfc,
                 /* result_unwrap_used, option_unwrap_used, */ // TODO(H2CO3): fix these
                 option_map_unwrap_or_else, option_map_unwrap_or, filter_map,
                 shadow_unrelated, shadow_reuse, shadow_same,
                 int_plus_one, string_add_assign, if_not_else,
                 invalid_upcast_comparisons,
                 cast_sign_loss, cast_precision_loss,
                 cast_possible_wrap, cast_possible_truncation,
                 mutex_integer, mut_mut, items_after_statements,
                 print_stdout, mem_forget, maybe_infinite_iter))]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate itertools;
extern crate regex;
extern crate phile;

mod common;

use common::*;
use phile::lexer::{ self, Token, TokenKind };
use phile::ast::*;
use phile::error::*;
use phile::parser;
use phile::util::{ grapheme_count, Location, Range };


fn lex_filter_ws_comment<S: AsRef<str>>(sources: &[S]) -> Vec<Token> {
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

fn test_invalid_cases(cases: &[InvalidTestCase]) {
    for case in cases {
        let (actual_message, actual_range) = parse_invalid(case.source);
        let expected_message = case.message;
        let expected_range = case.error_range();

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
        InvalidTestCase {
            source:  "struct Keyword { nil: SomeType, }",
            marker:  "                 ^__^            ",
            message: "Expected identifier; found nil",
        },
        InvalidTestCase {
            source:  "class NoGood { enum: OtherType, }",
            marker:  "               ^___^",
            message: "Expected identifier; found enum",
        },
        InvalidTestCase {
            source:  "struct KeywordType { hey: impl, }",
            marker:  "                          ^___^",
            message: "Expected a type; found impl",
        },
        InvalidTestCase {
            source:  "class NoGoodType { ouch: if, }",
            marker:  "                         ^_^  ",
            message: "Expected a type; found if",
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
        InvalidTestCase {
            source:  "enum KeywordVariant { fn, }",
            marker:  "                      ^_^",
            message: "Expected identifier; found fn",
        },
        InvalidTestCase {
            source:  "enum KeywordAssocType { Boo(as), }",
            marker:  "                            ^_^   ",
            message: "Expected a type; found as",
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

struct ValidFuncGenParams {
    args:           &'static [(&'static str, &'static str)],
    fn_name:        &'static str,
    ret_type_str:   &'static str,
    body_str:       &'static str,
    has_arg_type:   bool,
    has_ret_type:   bool,
    has_body:       bool,
    trailing_comma: bool,
}

// Helper for `valid_fn_def()`.
fn valid_fn_source_and_ast(params: &ValidFuncGenParams) -> (String, Function<'static>) {
    let mut buf = String::new();
    let &ValidFuncGenParams {
        args,
        fn_name,
        ret_type_str,
        body_str,
        has_arg_type,
        has_ret_type,
        has_body,
        trailing_comma,
    } = params;

    let ((arguments, ret_type, body), range) = range_by_appending_with(&mut buf, |buf| {
        let mut arguments = Vec::with_capacity(args.len());

        *buf += "fn ";
        *buf += fn_name;
        *buf += "(";

        for (i, &(arg_name, arg_type_name)) in args.iter().enumerate() {
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

            arguments.push(Argument {
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
    let args_name_type = &[("foo", "String"), ("bar", "float")];
    let args = (0..args_name_type.len() + 1).map(|n| &args_name_type[..n]);
    let fn_names = vec!["some_func", "_", "noname", "nonIdiomaticFunction"];
    let ret_type_strs = vec!["Quxy", "weirdType"];
    let body_strs = vec!["the_value", "AnotherValue", "_"];
    let arg_flags   = vec![false, true];
    let ret_flags   = vec![false, true];
    let body_flags  = vec![false, true];
    let comma_flags = vec![false, true];

    let it = iproduct!(args, fn_names, ret_type_strs, body_strs, arg_flags, comma_flags, ret_flags, body_flags);

    for tuple in it {
        let params = ValidFuncGenParams {
            args:           tuple.0,
            fn_name:        tuple.1,
            ret_type_str:   tuple.2,
            body_str:       tuple.3,
            has_arg_type:   tuple.4,
            has_ret_type:   tuple.5,
            has_body:       tuple.6,
            trailing_comma: tuple.7,
        };
        let (buf, func) = valid_fn_source_and_ast(&params);
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
                                kind: ExpKind::Int("42"),
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
                        Argument {
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
                        Argument {
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

// TODO(H2CO3): rewrite boxes using impl trait once stable
type RangeGen = Box<Fn(usize, std::ops::Range<usize>) -> Range>;
type EvalExpTest = Box<Fn(Vec<(&str, Exp)>) -> ()>;
type EvalTyTest = Box<Fn(Vec<(&str, Ty)>) -> ()>;

fn valid_expression_tester() -> (RangeGen, EvalExpTest) {
    // Wrap up every expression in the body of a minimal function.
    // `exp_range()` is a small helper that translates the ranges
    // as visually perceived by inspection of the `exprs` array
    // to the actual ranges in the AST. These will just be shifted
    // by the length of the header of the wrapping fn, `prefix`.
    let prefix = "fn _() { ";
    let suffix = " }";

    let exp_range = move |i, r: std::ops::Range<usize>| {
        let shift = grapheme_count(prefix);
        oneline_range(i, r.start + shift..r.end + shift)
    };

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
    // Cases are expressions to be parsed and the corresponding expected
    // AST node fragments. Each node fragment will be the only element
    // in the body block of one of the array of wrapping functions.
    let evaluate = move |cases: Vec<(&str, Exp)>| {
        let decorate_case = |(i, (src, node))| {
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
        };

        // Finally, parse the array of sources, and compare the resulting
        // (actual) items to the expected ones constructed above.
        let iter = cases.into_iter().enumerate().map(decorate_case);
        let (sources, expected): (Vec<_>, Vec<_>) = iter.unzip();
        let tokens = lex_filter_ws_comment(&sources);
        let actual = parse_valid(&tokens).items;

        assert_eq!(actual.len(), expected.len());

        for (actual, expected) in actual.into_iter().zip(expected) {
            assert_eq!(actual, expected);
        }
    };

    (Box::new(exp_range), Box::new(evaluate))
}

// This function is similar to valid_expression_tester(), but it
// returns functions suitable for testing type parsers instead.
fn valid_type_tester() -> (RangeGen, EvalTyTest) {
    let prefix = "fn _(_: ";
    let suffix = ") {}";

    let ty_range = move |i, r: std::ops::Range<usize>| {
        let shift = grapheme_count(prefix);
        oneline_range(i, r.start + shift..r.end + shift)
    };

    let evaluate = move |cases: Vec<(&str, Ty)>| {
        let decorate_case = |(i, (src, node))| {
            let src = [prefix, src, suffix].join("");
            let start_byte_index = src.find('(').unwrap() + 1;
            let end_byte_index = src.rfind(')').unwrap();
            let start = 1 + grapheme_count(&src[..start_byte_index]);
            let end = 1 + grapheme_count(&src[..end_byte_index]);
            let gr_count = grapheme_count(&src);

            // these are just necessary to construct the wrapper Item:
            // 1. range of the function argument (_: <Type>)
            let arg_range = oneline_range(i, start..end);
            // 2. range of the entire wrapper function
            let func_range = oneline_range(i, 1..gr_count + 1);
            // 3. range of the trailing "{}", the empty fn body
            let body_range = oneline_range(i, gr_count - 2 + 1..gr_count + 1);

            let item = Item::FuncDef(Function {
                range: func_range,
                name: Some("_"),
                arguments: vec![
                    Argument {
                        range: arg_range,
                        name: "_",
                        ty: Some(node),
                    },
                ],
                ret_type: None,
                body: Exp {
                    kind: ExpKind::Block(vec![]),
                    range: body_range,
                },
            });

            (src, item)
        };

        // Finally, parse the array of sources, and compare the resulting
        // (actual) items to the expected ones constructed above.
        let iter = cases.into_iter().enumerate().map(decorate_case);
        let (sources, expected): (Vec<_>, Vec<_>) = iter.unzip();
        let tokens = lex_filter_ws_comment(&sources);
        let actual = parse_valid(&tokens).items;

        assert_eq!(actual.len(), expected.len());

        for (actual, expected) in actual.into_iter().zip(expected) {
            assert_eq!(actual, expected);
        }
    };

    (Box::new(ty_range), Box::new(evaluate))
}

//
// Valid Expressions
//

#[test]
fn valid_atomic_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "nil",
            Exp {
                kind: ExpKind::Nil,
                range: exp_range(0, 1..4),
            },
        ),
        (
            "true",
            Exp {
                kind: ExpKind::Bool("true"),
                range: exp_range(1, 1..5),
            },
        ),
        (
            "false",
            Exp {
                kind: ExpKind::Bool("false"),
                range: exp_range(2, 1..6),
            },
        ),
        (
            "42",
            Exp {
                kind: ExpKind::Int("42"),
                range: exp_range(3, 1..3),
            },
        ),
        (
            "13.37",
            Exp {
                kind: ExpKind::Float("13.37"),
                range: exp_range(4, 1..6),
            },
        ),
        (
            r#""""#, // empty string
            Exp {
                kind: ExpKind::String(r#""""#),
                range: exp_range(5, 1..3),
            },
        ),
        (
            r#""not empty""#, // non-empty string
            Exp {
                kind: ExpKind::String(r#""not empty""#),
                range: exp_range(6, 1..12),
            },
        ),
        (
            r#""\"\\""#, // \"\\ (escaped escape char and quote)
            Exp {
                kind: ExpKind::String(r#""\"\\""#),
                range: exp_range(7, 1..7),
            },
        ),
        (
            "\"\u{6F22}\"", // Unicode (CJK) character
            Exp {
                kind: ExpKind::String("\"\u{6F22}\""),
                range: exp_range(8, 1..4),
            },
        ),
        (
            r#""\u{6F22}""#, // Unicode (CJK) character, escaped
            Exp {
                kind: ExpKind::String(r#""\u{6F22}""#),
                range: exp_range(9, 1..11),
            },
        ),
        (
            "_", // identifier
            Exp {
                kind: ExpKind::Identifier("_"),
                range: exp_range(10, 1..2),
            },
        ),
        (
            "lower_snake_case",
            Exp {
                kind: ExpKind::Identifier("lower_snake_case"),
                range: exp_range(11, 1..17),
            },
        ),
        (
            "UPPER_SNAKE_CASE",
            Exp {
                kind: ExpKind::Identifier("UPPER_SNAKE_CASE"),
                range: exp_range(12, 1..17),
            },
        ),
        (
            "lowerCamelCase",
            Exp {
                kind: ExpKind::Identifier("lowerCamelCase"),
                range: exp_range(13, 1..15),
            },
        ),
        (
            "UpperCamelCase",
            Exp {
                kind: ExpKind::Identifier("UpperCamelCase"),
                range: exp_range(14, 1..15),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_non_atomic_term() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "()",
            Exp {
                kind: ExpKind::Tuple(vec![]),
                range: exp_range(0, 1..3),
            },
        ),
        (
            "(())",
            Exp {
                kind: ExpKind::Tuple(vec![
                    Exp {
                        kind: ExpKind::Tuple(vec![]),
                        range: exp_range(1, 2..4),
                    },
                ]),
                range: exp_range(1, 1..5),
            },
        ),
        (
            "((),())",
            Exp {
                kind: ExpKind::Tuple(vec![
                    Exp {
                        kind: ExpKind::Tuple(vec![]),
                        range: exp_range(2, 2..4),
                    },
                    Exp {
                        kind: ExpKind::Tuple(vec![]),
                        range: exp_range(2, 5..7),
                    },
                ]),
                range: exp_range(2, 1..8),
            },
        ),
        (
            "(foo)",
            Exp {
                kind: ExpKind::Tuple(vec![
                    Exp {
                        kind: ExpKind::Identifier("foo"),
                        range: exp_range(3, 2..5),
                    },
                ]),
                range: exp_range(3, 1..6),
            },
        ),
        (
            "((nil),)",
            Exp {
                kind: ExpKind::Tuple(vec![
                    Exp {
                        kind: ExpKind::Tuple(vec![
                            Exp {
                                kind: ExpKind::Nil,
                                range: exp_range(4, 3..6),
                            },
                        ]),
                        range: exp_range(4, 2..7),
                    },
                ]),
                range: exp_range(4, 1..9),
            },
        ),
        (
            "(1, 0, 0.0, 99.56)",
            Exp {
                kind: ExpKind::Tuple(vec![
                    Exp {
                        kind: ExpKind::Int("1"),
                        range: exp_range(5, 2..3),
                    },
                    Exp {
                        kind: ExpKind::Int("0"),
                        range: exp_range(5, 5..6),
                    },
                    Exp {
                        kind: ExpKind::Float("0.0"),
                        range: exp_range(5, 8..11),
                    },
                    Exp {
                        kind: ExpKind::Float("99.56"),
                        range: exp_range(5, 13..18),
                    },
                ]),
                range: exp_range(5, 1..19),
            },
        ),
        (
            "[]",
            Exp {
                kind: ExpKind::Array(vec![]),
                range: exp_range(6, 1..3),
            },
        ),
        (
            "[[]]",
            Exp {
                kind: ExpKind::Array(vec![
                    Exp {
                        kind: ExpKind::Array(vec![]),
                        range: exp_range(7, 2..4),
                    },
                ]),
                range: exp_range(7, 1..5),
            },
        ),
        (
            "[[],[]]",
            Exp {
                kind: ExpKind::Array(vec![
                    Exp {
                        kind: ExpKind::Array(vec![]),
                        range: exp_range(8, 2..4),
                    },
                    Exp {
                        kind: ExpKind::Array(vec![]),
                        range: exp_range(8, 5..7),
                    },
                ]),
                range: exp_range(8, 1..8),
            },
        ),
        (
            "[[true, false, ]]",
            Exp {
                kind: ExpKind::Array(vec![
                    Exp {
                        kind: ExpKind::Array(vec![
                            Exp {
                                kind: ExpKind::Bool("true"),
                                range: exp_range(9, 3..7),
                            },
                            Exp {
                                kind: ExpKind::Bool("false"),
                                range: exp_range(9, 9..14),
                            },
                        ]),
                        range: exp_range(9, 2..17),
                    },
                ]),
                range: exp_range(9, 1..18),
            },
        ),
        (
            "{}",
            Exp {
                kind: ExpKind::Block(vec![]),
                range: exp_range(10, 1..3),
            },
        ),
        (
            "{{}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: exp_range(11, 2..4),
                    },
                ]),
                range: exp_range(11, 1..5),
            },
        ),
        (
            "{{}{}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: exp_range(12, 2..4),
                    },
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: exp_range(12, 4..6),
                    },
                ]),
                range: exp_range(12, 1..7),
            },
        ),
        (
            "{{};{}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: exp_range(13, 2..4),
                    },
                    Exp {
                        kind: ExpKind::Empty,
                        range: exp_range(13, 4..5),
                    },
                    Exp {
                        kind: ExpKind::Block(vec![]),
                        range: exp_range(13, 5..7),
                    },
                ]),
                range: exp_range(13, 1..8),
            },
        ),
        (
            "{;}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Empty,
                        range: exp_range(14, 2..3),
                    },
                ]),
                range: exp_range(14, 1..4),
            },
        ),
        (
            "{3.1415927; {()}}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Semi(
                            Box::new(Exp {
                                kind: ExpKind::Float("3.1415927"),
                                range: exp_range(15, 2..11),
                            })
                        ),
                        range: exp_range(15, 2..11),
                    },
                    Exp {
                        kind: ExpKind::Block(vec![
                            Exp {
                                kind: ExpKind::Tuple(vec![]),
                                range: exp_range(15, 14..16),
                            },
                        ]),
                        range: exp_range(15, 13..17),
                    },
                ]),
                range: exp_range(15, 1..18),
            },
        ),
        (
            "{0;;}",
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::Semi(
                            Box::new(Exp {
                                kind: ExpKind::Int("0"),
                                range: exp_range(16, 2..3),
                            })
                        ),
                        range: exp_range(16, 2..3),
                    },
                    Exp {
                        kind: ExpKind::Empty,
                        range: exp_range(16, 4..5),
                    },
                ]),
                range: exp_range(16, 1..6),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_variable_decl() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "{ let var_name = stuff; }", // variable declaration without explicit types
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::VarDecl(
                            Box::new(VarDecl {
                                name: "var_name",
                                ty: None,
                                expr: Exp {
                                    kind: ExpKind::Identifier("stuff"),
                                    range: exp_range(0, 18..23),
                                },
                            })
                        ),
                        range: exp_range(0, 3..24),
                    },
                ]),
                range: exp_range(0, 1..26),
            },
        ),
        (
            "{ let x: T = 999; }", // variable declaration with type annotation
            Exp {
                kind: ExpKind::Block(vec![
                    Exp {
                        kind: ExpKind::VarDecl(
                            Box::new(VarDecl {
                                name: "x",
                                ty: Some(Ty {
                                    kind: TyKind::Named("T"),
                                    range: exp_range(1, 10..11),
                                }),
                                expr: Exp {
                                    kind: ExpKind::Int("999"),
                                    range: exp_range(1, 14..17),
                                },
                            })
                        ),
                        range: exp_range(1, 3..18),
                    },
                ]),
                range: exp_range(1, 1..20),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_if_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "if cond { }", // 'if' without else branch
            Exp {
                kind: ExpKind::If(
                    Box::new(If {
                        condition: Exp {
                            kind: ExpKind::Identifier("cond"),
                            range: exp_range(0, 4..8),
                        },
                        then_arm: Exp {
                            kind: ExpKind::Block(vec![]),
                            range: exp_range(0, 9..12),
                        },
                        else_arm: None,
                    })
                ),
                range: exp_range(0, 1..12),
            },
        ),
        (
            "if true {} else {}", // if with else block
            Exp {
                kind: ExpKind::If(
                    Box::new(If {
                        condition: Exp {
                            kind: ExpKind::Bool("true"),
                            range: exp_range(1, 4..8),
                        },
                        then_arm: Exp {
                            kind: ExpKind::Block(vec![]),
                            range: exp_range(1, 9..11),
                        },
                        else_arm: Some(Exp {
                            kind: ExpKind::Block(vec![]),
                            range: exp_range(1, 17..19),
                        }),
                    })
                ),
                range: exp_range(1, 1..19),
            },
        ),
        (
            "if false {} else if true {} else {}", // else if
            Exp {
                kind: ExpKind::If(
                    Box::new(If {
                        condition: Exp {
                            kind: ExpKind::Bool("false"),
                            range: exp_range(2, 4..9),
                        },
                        then_arm: Exp {
                            kind: ExpKind::Block(vec![]),
                            range: exp_range(2, 10..12),
                        },
                        else_arm: Some(Exp {
                            kind: ExpKind::If(
                                Box::new(If {
                                    condition: Exp {
                                        kind: ExpKind::Bool("true"),
                                        range: exp_range(2, 21..25),
                                    },
                                    then_arm: Exp {
                                        kind: ExpKind::Block(vec![]),
                                        range: exp_range(2, 26..28),
                                    },
                                    else_arm: Some(Exp {
                                        kind: ExpKind::Block(vec![]),
                                        range: exp_range(2, 34..36),
                                    }),
                                })
                            ),
                            range: exp_range(2, 18..36),
                        }),
                    })
                ),
                range: exp_range(2, 1..36),
            },
        ),
        (
            "if this { if that {} }", // nested
            Exp {
                kind: ExpKind::If(
                    Box::new(If {
                        condition: Exp {
                            kind: ExpKind::Identifier("this"),
                            range: exp_range(3, 4..8),
                        },
                        then_arm: Exp {
                            kind: ExpKind::Block(vec![
                                Exp {
                                    kind: ExpKind::If(
                                        Box::new(If {
                                            condition: Exp {
                                                kind: ExpKind::Identifier("that"),
                                                range: exp_range(3, 14..18),
                                            },
                                            then_arm: Exp {
                                                kind: ExpKind::Block(vec![]),
                                                range: exp_range(3, 19..21),
                                            },
                                            else_arm: None,
                                        })
                                    ),
                                    range: exp_range(3, 11..21),
                                },
                            ]),
                            range: exp_range(3, 9..23),
                        },
                        else_arm: None,
                    })
                ),
                range: exp_range(3, 1..23),
            },
        ),
        (
            "if those {} else { if these {} }", // more nested
            Exp {
                kind: ExpKind::If(
                    Box::new(If {
                        condition: Exp {
                            kind: ExpKind::Identifier("those"),
                            range: exp_range(4, 4..9),
                        },
                        then_arm: Exp {
                            kind: ExpKind::Block(vec![]),
                            range: exp_range(4, 10..12),
                        },
                        else_arm: Some(Exp {
                            kind: ExpKind::Block(vec![
                                Exp {
                                    kind: ExpKind::If(
                                        Box::new(If {
                                            condition: Exp {
                                                kind: ExpKind::Identifier("these"),
                                                range: exp_range(4, 23..28),
                                            },
                                            then_arm: Exp {
                                                kind: ExpKind::Block(vec![]),
                                                range: exp_range(4, 29..31),
                                            },
                                            else_arm: None,
                                        })
                                    ),
                                    range: exp_range(4, 20..31),
                                },
                            ]),
                            range: exp_range(4, 18..33),
                        }),
                    })
                ),
                range: exp_range(4, 1..33),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_closure() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "|| _", // function expression with no arguments
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(0, 1..5),
                        name: None,
                        arguments: vec![],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(0, 4..5),
                        },
                    })
                ),
                range: exp_range(0, 1..5),
            },
        ),
        (
            "|a| _", // one argument, no type annotations
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(1, 1..6),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(1, 2..3),
                                name: "a",
                                ty: None,
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(1, 5..6),
                        },
                    })
                ),
                range: exp_range(1, 1..6),
            },
        ),
        (
            "|foo,| {}", // one argument without type annotation and a trailing comma
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(2, 1..10),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(2, 2..5),
                                name: "foo",
                                ty: None,
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Block(vec![]),
                            range: exp_range(2, 8..10),
                        },
                    })
                ),
                range: exp_range(2, 1..10),
            },
        ),
        (
            "|a, b| _", // multiple arguments, no type annotations
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(3, 1..9),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(3, 2..3),
                                name: "a",
                                ty: None,
                            },
                            Argument {
                                range: exp_range(3, 5..6),
                                name: "b",
                                ty: None,
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(3, 8..9),
                        },
                    })
                ),
                range: exp_range(3, 1..9),
            },
        ),
        (
            "|a, b,| _", // multiple arguments and a trailing comma, no type annotations
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(4, 1..10),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(4, 2..3),
                                name: "a",
                                ty: None,
                            },
                            Argument {
                                range: exp_range(4, 5..6),
                                name: "b",
                                ty: None,
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(4, 9..10),
                        },
                    })
                ),
                range: exp_range(4, 1..10),
            },
        ),
        (
            "|a: T| _", // one argument with type annotations
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(5, 1..9),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(5, 2..6),
                                name: "a",
                                ty: Some(Ty {
                                    kind: TyKind::Named("T"),
                                    range: exp_range(5, 5..6),
                                }),
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(5, 8..9),
                        },
                    })
                ),
                range: exp_range(5, 1..9),
            },
        ),
        (
            "|b: U,| _", // one argument with type annotations and a trailing comma
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(6, 1..10),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(6, 2..6),
                                name: "b",
                                ty: Some(Ty {
                                    kind: TyKind::Named("U"),
                                    range: exp_range(6, 5..6),
                                }),
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(6, 9..10),
                        },
                    })
                ),
                range: exp_range(6, 1..10),
            },
        ),
        (
            "|a: T, b: U| _", // many arguments with type annotations
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(7, 1..15),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(7, 2..6),
                                name: "a",
                                ty: Some(Ty {
                                    kind: TyKind::Named("T"),
                                    range: exp_range(7, 5..6),
                                }),
                            },
                            Argument {
                                range: exp_range(7, 8..12),
                                name: "b",
                                ty: Some(Ty {
                                    kind: TyKind::Named("U"),
                                    range: exp_range(7, 11..12),
                                }),
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(7, 14..15),
                        },
                    })
                ),
                range: exp_range(7, 1..15),
            },
        ),
        (
            "|a: T, b: U,| _", // many arguments with type annotations and trailing comma
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(8, 1..16),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(8, 2..6),
                                name: "a",
                                ty: Some(Ty {
                                    kind: TyKind::Named("T"),
                                    range: exp_range(8, 5..6),
                                }),
                            },
                            Argument {
                                range: exp_range(8, 8..12),
                                name: "b",
                                ty: Some(Ty {
                                    kind: TyKind::Named("U"),
                                    range: exp_range(8, 11..12),
                                }),
                            },
                        ],
                        ret_type: None,
                        body: Exp {
                            kind: ExpKind::Identifier("_"),
                            range: exp_range(8, 15..16),
                        },
                    })
                ),
                range: exp_range(8, 1..16),
            },
        ),
        (
            "|| -> [Z] {}", // no argument and an explicit return type
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(9, 1..13),
                        name: None,
                        arguments: vec![],
                        ret_type: Some(Ty {
                            kind: TyKind::Array(
                                Box::new(Ty {
                                    kind: TyKind::Named("Z"),
                                    range: exp_range(9, 8..9),
                                })
                            ),
                            range: exp_range(9, 7..10),
                        }),
                        body: Exp {
                            kind: ExpKind::Block(vec![]),
                            range: exp_range(9, 11..13),
                        },
                    })
                ),
                range: exp_range(9, 1..13),
            },
        ),
        (
            "|arg_name| -> Meh? { body }", // one argument and an explicit return type
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(10, 1..28),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(10, 2..10),
                                name: "arg_name",
                                ty: None,
                            },
                        ],
                        ret_type: Some(Ty {
                            kind: TyKind::Optional(
                                Box::new(Ty {
                                    kind: TyKind::Named("Meh"),
                                    range: exp_range(10, 15..18),
                                })
                            ),
                            range: exp_range(10, 15..19),
                        }),
                        body: Exp {
                            kind: ExpKind::Block(vec![
                                Exp {
                                    kind: ExpKind::Identifier("body"),
                                    range: exp_range(10, 22..26),
                                },
                            ]),
                            range: exp_range(10, 20..28),
                        },
                    })
                ),
                range: exp_range(10, 1..28),
            },
        ),
        (
            "|x: &ABC| -> (_) { _ }", // explicit argument type and return type
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(11, 1..23),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(11, 2..9),
                                name: "x",
                                ty: Some(Ty {
                                    kind: TyKind::Pointer(
                                        Box::new(Ty {
                                            kind: TyKind::Named("ABC"),
                                            range: exp_range(11, 6..9),
                                        }),
                                    ),
                                    range: exp_range(11, 5..9),
                                }),
                            },
                        ],
                        ret_type: Some(Ty {
                            kind: TyKind::Tuple(vec![
                                Ty {
                                    kind: TyKind::Named("_"),
                                    range: exp_range(11, 15..16),
                                },
                            ]),
                            range: exp_range(11, 14..17),
                        }),
                        body: Exp {
                            kind: ExpKind::Block(vec![
                                Exp {
                                    kind: ExpKind::Identifier("_"),
                                    range: exp_range(11, 20..21),
                                },
                            ]),
                            range: exp_range(11, 18..23),
                        },
                    })
                ),
                range: exp_range(11, 1..23),
            },
        ),
        (
            "|_0, _1| -> _2 { _3 }", // multiple arguments and an explicit return type
            Exp {
                kind: ExpKind::FuncExp(
                    Box::new(Function {
                        range: exp_range(12, 1..22),
                        name: None,
                        arguments: vec![
                            Argument {
                                range: exp_range(12, 2..4),
                                name: "_0",
                                ty: None,
                            },
                            Argument {
                                range: exp_range(12, 6..8),
                                name: "_1",
                                ty: None,
                            },
                        ],
                        ret_type: Some(Ty {
                            kind: TyKind::Named("_2"),
                            range: exp_range(12, 13..15),
                        }),
                        body: Exp {
                            kind: ExpKind::Block(vec![
                                Exp {
                                    kind: ExpKind::Identifier("_3"),
                                    range: exp_range(12, 18..20),
                                },
                            ]),
                            range: exp_range(12, 16..22),
                        },
                    })
                ),
                range: exp_range(12, 1..22),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_member_access() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "base.member",
            Exp {
                kind: ExpKind::MemberAccess(MemberAccess {
                    base: Box::new(Exp {
                        kind: ExpKind::Identifier("base"),
                        range: exp_range(0, 1..5),
                    }),
                    member: "member",
                }),
                range: exp_range(0, 1..12),
            },
        ),
        (
            "(base_expr,).member_name.multi",
            Exp {
                kind: ExpKind::MemberAccess(MemberAccess {
                    base: Box::new(Exp {
                        kind: ExpKind::MemberAccess(MemberAccess {
                            base: Box::new(Exp {
                                kind: ExpKind::Tuple(vec![
                                    Exp {
                                        kind: ExpKind::Identifier("base_expr"),
                                        range: exp_range(1, 2..11),
                                    },
                                ]),
                                range: exp_range(1, 1..13),
                            }),
                            member: "member_name",
                        }),
                        range: exp_range(1, 1..25),
                    }),
                    member: "multi",
                }),
                range: exp_range(1, 1..31),
            },
        ),
        (
            "namespace::Item",
            Exp {
                kind: ExpKind::QualAccess(QualAccess {
                    base: Box::new(Exp {
                        kind: ExpKind::Identifier("namespace"),
                        range: exp_range(2, 1..10),
                    }),
                    member: "Item",
                }),
                range: exp_range(2, 1..16),
            },
        ),
        (
            "[other_namespace]::Deeper::OtherItem",
            Exp {
                kind: ExpKind::QualAccess(QualAccess {
                    base: Box::new(Exp {
                        kind: ExpKind::QualAccess(QualAccess {
                            base: Box::new(Exp {
                                kind: ExpKind::Array(vec![
                                    Exp {
                                        kind: ExpKind::Identifier("other_namespace"),
                                        range: exp_range(3, 2..17),
                                    },
                                ]),
                                range: exp_range(3, 1..18),
                            }),
                            member: "Deeper",
                        }),
                        range: exp_range(3, 1..26),
                    }),
                    member: "OtherItem",
                }),
                range: exp_range(3, 1..37),
            },
        ),
        (
            "mixed.foo::bar.expr",
            Exp {
                kind: ExpKind::MemberAccess(MemberAccess {
                    base: Box::new(Exp {
                        kind: ExpKind::QualAccess(QualAccess {
                            base: Box::new(Exp {
                                kind: ExpKind::MemberAccess(MemberAccess {
                                    base: Box::new(Exp {
                                        kind: ExpKind::Identifier("mixed"),
                                        range: exp_range(4, 1..6),
                                    }),
                                    member: "foo",
                                }),
                                range: exp_range(4, 1..10),
                            }),
                            member: "bar",
                        }),
                        range: exp_range(4, 1..15),
                    }),
                    member: "expr",
                }),
                range: exp_range(4, 1..20),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_subscript() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "sub[script]",
            Exp {
                kind: ExpKind::Subscript(
                    Box::new(Subscript {
                        base: Exp {
                            kind: ExpKind::Identifier("sub"),
                            range: exp_range(0, 1..4),
                        },
                        index: Exp {
                            kind: ExpKind::Identifier("script"),
                            range: exp_range(0, 5..11),
                        },
                    })
                ),
                range: exp_range(0, 1..12),
            },
        ),
        (
            r#"more[77]["subscripting"]"#,
            Exp {
                kind: ExpKind::Subscript(
                    Box::new(Subscript {
                        base: Exp {
                            kind: ExpKind::Subscript(
                                Box::new(Subscript {
                                    base: Exp {
                                        kind: ExpKind::Identifier("more"),
                                        range: exp_range(1, 1..5),
                                    },
                                    index: Exp {
                                        kind: ExpKind::Int("77"),
                                        range: exp_range(1, 6..8),
                                    },
                                })
                            ),
                            range: exp_range(1, 1..9),
                        },
                        index: Exp {
                            kind: ExpKind::String(r#""subscripting""#),
                            range: exp_range(1, 10..24),
                        },
                    })
                ),
                range: exp_range(1, 1..25),
            },
        ),
        (
            "sub[script[ception]]",
            Exp {
                kind: ExpKind::Subscript(
                    Box::new(Subscript {
                        base: Exp {
                            kind: ExpKind::Identifier("sub"),
                            range: exp_range(2, 1..4),
                        },
                        index: Exp {
                            kind: ExpKind::Subscript(
                                Box::new(Subscript {
                                    base: Exp {
                                        kind: ExpKind::Identifier("script"),
                                        range: exp_range(2, 5..11),
                                    },
                                    index: Exp {
                                        kind: ExpKind::Identifier("ception"),
                                        range: exp_range(2, 12..19),
                                    },
                                }),
                            ),
                            range: exp_range(2, 5..20),
                        },
                    })
                ),
                range: exp_range(2, 1..21),
            },
        ),
        (
            "[][[]]",
            Exp {
                kind: ExpKind::Subscript(
                    Box::new(Subscript {
                        base: Exp {
                            kind: ExpKind::Array(vec![]),
                            range: exp_range(3, 1..3),
                        },
                        index: Exp {
                            kind: ExpKind::Array(vec![]),
                            range: exp_range(3, 4..6),
                        },
                    })
                ),
                range: exp_range(3, 1..7),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_function_call() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "no_arg()",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Identifier("no_arg"),
                        range: exp_range(0, 1..7),
                    }),
                    arguments: vec![],
                }),
                range: exp_range(0, 1..9),
            },
        ),
        (
            "single_arg(nil)",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Identifier("single_arg"),
                        range: exp_range(1, 1..11),
                    }),
                    arguments: vec![
                        Exp {
                            kind: ExpKind::Nil,
                            range: exp_range(1, 12..15),
                        },
                    ],
                }),
                range: exp_range(1, 1..16),
            },
        ),
        (
            "single_arg(welp,)",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Identifier("single_arg"),
                        range: exp_range(2, 1..11),
                    }),
                    arguments: vec![
                        Exp {
                            kind: ExpKind::Identifier("welp"),
                            range: exp_range(2, 12..16),
                        },
                    ],
                }),
                range: exp_range(2, 1..18),
            },
        ),
        (
            "multi_arg_1(0.5, true)",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Identifier("multi_arg_1"),
                        range: exp_range(3, 1..12),
                    }),
                    arguments: vec![
                        Exp {
                            kind: ExpKind::Float("0.5"),
                            range: exp_range(3, 13..16),
                        },
                        Exp {
                            kind: ExpKind::Bool("true"),
                            range: exp_range(3, 18..22),
                        },
                    ],
                }),
                range: exp_range(3, 1..23),
            },
        ),
        (
            "multi_arg_2(nil, null,)",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Identifier("multi_arg_2"),
                        range: exp_range(4, 1..12),
                    }),
                    arguments: vec![
                        Exp {
                            kind: ExpKind::Nil,
                            range: exp_range(4, 13..16),
                        },
                        Exp {
                            kind: ExpKind::Identifier("null"),
                            range: exp_range(4, 18..22),
                        },
                    ],
                }),
                range: exp_range(4, 1..24),
            },
        ),
        (
            "call_call()()",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Call(Call {
                            function: Box::new(Exp {
                                kind: ExpKind::Identifier("call_call"),
                                range: exp_range(5, 1..10),
                            }),
                            arguments: vec![],
                        }),
                        range: exp_range(5, 1..12),
                    }),
                    arguments: vec![],
                }),
                range: exp_range(5, 1..14),
            },
        ),
        (
            "nested(call())",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Identifier("nested"),
                        range: exp_range(6, 1..7),
                    }),
                    arguments: vec![
                        Exp {
                            kind: ExpKind::Call(Call {
                                function: Box::new(Exp {
                                    kind: ExpKind::Identifier("call"),
                                    range: exp_range(6, 8..12),
                                }),
                                arguments: vec![],
                            }),
                            range: exp_range(6, 8..14),
                        },
                    ],
                }),
                range: exp_range(6, 1..15),
            },
        ),
        (
            "(parenthesized_call())",
            Exp {
                kind: ExpKind::Tuple(vec![
                    Exp {
                        kind: ExpKind::Call(Call {
                            function: Box::new(Exp {
                                kind: ExpKind::Identifier("parenthesized_call"),
                                range: exp_range(7, 2..20),
                            }),
                            arguments: vec![],
                        }),
                        range: exp_range(7, 2..22),
                    },
                ]),
                range: exp_range(7, 1..23),
            },
        ),
        (
            "(parenthesized_callee)()",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Tuple(vec![
                            Exp {
                                kind: ExpKind::Identifier("parenthesized_callee"),
                                range: exp_range(8, 2..22),
                            },
                        ]),
                        range: exp_range(8, 1..23),
                    }),
                    arguments: vec![],
                }),
                range: exp_range(8, 1..25),
            },
        ),
        (
            "((callception)())()()",
            Exp {
                kind: ExpKind::Call(Call {
                    function: Box::new(Exp {
                        kind: ExpKind::Call(Call {
                            function: Box::new(Exp {
                                kind: ExpKind::Tuple(vec![
                                    Exp {
                                        kind: ExpKind::Call(Call {
                                            function: Box::new(Exp {
                                                kind: ExpKind::Tuple(vec![
                                                    Exp {
                                                        kind: ExpKind::Identifier("callception"),
                                                        range: exp_range(9, 3..14),
                                                    },
                                                ]),
                                                range: exp_range(9, 2..15),
                                            }),
                                            arguments: vec![],
                                        }),
                                        range: exp_range(9, 2..17),
                                    },

                                ]),
                                range: exp_range(9, 1..18),
                            }),
                            arguments: vec![],
                        }),
                        range: exp_range(9, 1..20),
                    }),
                    arguments: vec![],
                }),
                range: exp_range(9, 1..22),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_prefix_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "+abcdefgh",
            Exp {
                kind: ExpKind::UnaryPlus(
                    Box::new(Exp {
                        kind: ExpKind::Identifier("abcdefgh"),
                        range: exp_range(0, 2..10),
                    })
                ),
                range: exp_range(0, 1..10),
            },
        ),
        (
            "-nil",
            Exp {
                kind: ExpKind::UnaryMinus(
                    Box::new(Exp {
                        kind: ExpKind::Nil,
                        range: exp_range(1, 2..5),
                    })
                ),
                range: exp_range(1, 1..5),
            },
        ),
        (
            "~8438",
            Exp {
                kind: ExpKind::LogicNot(
                    Box::new(Exp {
                        kind: ExpKind::Int("8438"),
                        range: exp_range(2, 2..6),
                    })
                ),
                range: exp_range(2, 1..6),
            },
        ),
        (
            "++00",
            Exp {
                kind: ExpKind::UnaryPlus(
                    Box::new(Exp {
                        kind: ExpKind::UnaryPlus(
                            Box::new(Exp {
                                kind: ExpKind::Int("00"),
                                range: exp_range(3, 3..5),
                            })
                        ),
                        range: exp_range(3, 2..5),
                    })
                ),
                range: exp_range(3, 1..5),
            },
        ),
        (
            "--49.06",
            Exp {
                kind: ExpKind::UnaryMinus(
                    Box::new(Exp {
                        kind: ExpKind::UnaryMinus(
                            Box::new(Exp {
                                kind: ExpKind::Float("49.06"),
                                range: exp_range(4, 3..8),
                            })
                        ),
                        range: exp_range(4, 2..8),
                    })
                ),
                range: exp_range(4, 1..8),
            },
        ),
        (
            "~~false",
            Exp {
                kind: ExpKind::LogicNot(
                    Box::new(Exp {
                        kind: ExpKind::LogicNot(
                            Box::new(Exp {
                                kind: ExpKind::Bool("false"),
                                range: exp_range(5, 3..8),
                            })
                        ),
                        range: exp_range(5, 2..8),
                    })
                ),
                range: exp_range(5, 1..8),
            },
        ),
        (
            "-+~-~+x",
            Exp {
                kind: ExpKind::UnaryMinus(
                    Box::new(Exp {
                        kind: ExpKind::UnaryPlus(
                            Box::new(Exp {
                                kind: ExpKind::LogicNot(
                                    Box::new(Exp {
                                        kind: ExpKind::UnaryMinus(
                                            Box::new(Exp {
                                                kind: ExpKind::LogicNot(
                                                    Box::new(Exp {
                                                        kind: ExpKind::UnaryPlus(
                                                            Box::new(Exp {
                                                                kind: ExpKind::Identifier("x"),
                                                                range: exp_range(6, 7..8),
                                                            })
                                                        ),
                                                        range: exp_range(6, 6..8),
                                                    })
                                                ),
                                                range: exp_range(6, 5..8),
                                            })
                                        ),
                                        range: exp_range(6, 4..8),
                                    })
                                ),
                                range: exp_range(6, 3..8),
                            })
                        ),
                        range: exp_range(6, 2..8),
                    })
                ),
                range: exp_range(6, 1..8),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_cast_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "expr as Type",
            Exp {
                kind: ExpKind::Cast(
                    Box::new(Exp {
                        kind: ExpKind::Identifier("expr"),
                        range: exp_range(0, 1..5),
                    }),
                    Ty {
                        kind: TyKind::Named("Type"),
                        range: exp_range(0, 9..13),
                    },
                ),
                range: exp_range(0, 1..13),
            },
        ),
        (
            "36 as float as TUV",
            Exp {
                kind: ExpKind::Cast(
                    Box::new(Exp {
                        kind: ExpKind::Cast(
                            Box::new(Exp {
                                kind: ExpKind::Int("36"),
                                range: exp_range(1, 1..3),
                            }),
                            Ty {
                                kind: TyKind::Named("float"),
                                range: exp_range(1, 7..12),
                            },
                        ),
                        range: exp_range(1, 1..12),
                    }),
                    Ty {
                        kind: TyKind::Named("TUV"),
                        range: exp_range(1, 16..19),
                    },
                ),
                range: exp_range(1, 1..19),
            },
        ),
    ];

    evaluate(cases);
}

// The next few test cases exercise parsing binary infix operators.
// We test every pair of consecutive precedence levels.
// This should be sufficient as long as the infix operator
// parser is reasonably well-factored, because ordering of
// precedence is transitive and total.
// For example, the parser is currently implemented as a
// systematic chain of calls to parse_binop_{left|no}assoc()
// with identical structure, but it could be a purely
// data-oriented, precedence-table-driven Pratt parser too,
// for instance; it would still satisfy the above requirement.
// In addition, we also test associativity and every
// possible operator at each individual precedence level.

#[test]
fn valid_multiplicative_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "15 * 823 as int", // higher precedence on RHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "*",
                        lhs: Exp {
                            kind: ExpKind::Int("15"),
                            range: exp_range(0, 1..3),
                        },
                        rhs: Exp {
                            kind: ExpKind::Cast(
                                Box::new(Exp {
                                    kind: ExpKind::Int("823"),
                                    range: exp_range(0, 6..9),
                                }),
                                Ty {
                                    kind: TyKind::Named("int"),
                                    range: exp_range(0, 13..16),
                                },
                            ),
                            range: exp_range(0, 6..16),
                        },
                    })
                ),
                range: exp_range(0, 1..16),
            },
        ),
        (
            "1 as float / 2049", // higher precedence on LHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "/",
                        lhs: Exp {
                            kind: ExpKind::Cast(
                                Box::new(Exp {
                                    kind: ExpKind::Int("1"),
                                    range: exp_range(1, 1..2),
                                }),
                                Ty {
                                    kind: TyKind::Named("float"),
                                    range: exp_range(1, 6..11),
                                },
                            ),
                            range: exp_range(1, 1..11),
                        },
                        rhs: Exp {
                            kind: ExpKind::Int("2049"),
                            range: exp_range(1, 14..18),
                        },
                    })
                ),
                range: exp_range(1, 1..18),
            },
        ),
        (
            "div % mod * 100 / 0.125", // associativity
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "/",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "*",
                                    lhs: Exp {
                                        kind: ExpKind::BinaryOp(
                                            Box::new(BinaryOp {
                                                op: "%",
                                                lhs: Exp {
                                                    kind: ExpKind::Identifier("div"),
                                                    range: exp_range(2, 1..4),
                                                },
                                                rhs: Exp {
                                                    kind: ExpKind::Identifier("mod"),
                                                    range: exp_range(2, 7..10),
                                                },
                                            })
                                        ),
                                        range: exp_range(2, 1..10),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("100"),
                                        range: exp_range(2, 13..16),
                                    },
                                })
                            ),
                            range: exp_range(2, 1..16),
                        },
                        rhs: Exp {
                            kind: ExpKind::Float("0.125"),
                            range: exp_range(2, 19..24),
                        },
                    })
                ),
                range: exp_range(2, 1..24),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_additive_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "false / nil + true", // higher precedence on LHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "+",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "/",
                                    lhs: Exp {
                                        kind: ExpKind::Bool("false"),
                                        range: exp_range(0, 1..6),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Nil,
                                        range: exp_range(0, 9..12),
                                    },
                                })
                            ),
                            range: exp_range(0, 1..12),
                        },
                        rhs: Exp {
                            kind: ExpKind::Bool("true"),
                            range: exp_range(0, 15..19),
                        },
                    })
                ),
                range: exp_range(0, 1..19),
            },
        ),
        (
            "some + any * none", // higher precedence on RHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "+",
                        lhs: Exp {
                            kind: ExpKind::Identifier("some"),
                            range: exp_range(1, 1..5),
                        },
                        rhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "*",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("any"),
                                        range: exp_range(1, 8..11),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("none"),
                                        range: exp_range(1, 14..18),
                                    },
                                })
                            ),
                            range: exp_range(1, 8..18),
                        },
                    })
                ),
                range: exp_range(1, 1..18),
            },
        ),
        (
            "1 - 2 + 3", // associativity
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "+",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "-",
                                    lhs: Exp {
                                        kind: ExpKind::Int("1"),
                                        range: exp_range(2, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("2"),
                                        range: exp_range(2, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(2, 1..6),
                        },
                        rhs: Exp {
                            kind: ExpKind::Int("3"),
                            range: exp_range(2, 9..10),
                        },
                    })
                ),
                range: exp_range(2, 1..10),
            },
        ),
        (
            "4 + 5 - 6", // + and - are have the same precedence; their order should not affect associativity
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "-",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "+",
                                    lhs: Exp {
                                        kind: ExpKind::Int("4"),
                                        range: exp_range(3, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("5"),
                                        range: exp_range(3, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(3, 1..6),
                        },
                        rhs: Exp {
                            kind: ExpKind::Int("6"),
                            range: exp_range(3, 9..10),
                        },
                    })
                ),
                range: exp_range(3, 1..10),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_comparison_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    macro_rules! case_for_higher_lhs_precedence {
        ($i: expr => $op: expr) => {
            (
                concat!("x - 9876.54321", $op, "0"),
                Exp {
                    kind: ExpKind::BinaryOp(
                        Box::new(BinaryOp {
                            op: $op,
                            lhs: Exp {
                                kind: ExpKind::BinaryOp(
                                    Box::new(BinaryOp {
                                        op: "-",
                                        lhs: Exp {
                                            kind: ExpKind::Identifier("x"),
                                            range: exp_range($i * 2 + 0, 1..2),
                                        },
                                        rhs: Exp {
                                            kind: ExpKind::Float("9876.54321"),
                                            range: exp_range($i * 2 + 0, 5..15),
                                        },
                                    })
                                ),
                                range: exp_range($i * 2 + 0, 1..15),
                            },
                            rhs: Exp {
                                kind: ExpKind::Int("0"),
                                range: exp_range($i * 2 + 0, 15 + $op.len()..16 + $op.len()),
                            },
                        })
                    ),
                    range: exp_range($i * 2 + 0, 1..16 + $op.len()),
                },
            )
        }
    }

    macro_rules! case_for_higher_rhs_precedence {
        ($i: expr => $op: expr) => {
            (
                concat!("0.0", $op, "a + 9999"),
                Exp {
                    kind: ExpKind::BinaryOp(
                        Box::new(BinaryOp {
                            op: $op,
                            lhs: Exp {
                                kind: ExpKind::Float("0.0"),
                                range: exp_range($i * 2 + 1, 1..4),
                            },
                            rhs: Exp {
                                kind: ExpKind::BinaryOp(
                                    Box::new(BinaryOp {
                                        op: "+",
                                        lhs: Exp {
                                            kind: ExpKind::Identifier("a"),
                                            range: exp_range($i * 2 + 1, 4 + $op.len()..5 + $op.len()),
                                        },
                                        rhs: Exp {
                                            kind: ExpKind::Int("9999"),
                                            range: exp_range($i * 2 + 1, 8 + $op.len()..12 + $op.len()),
                                        },
                                    })
                                ),
                                range: exp_range($i * 2 + 1, 4 + $op.len()..12 + $op.len()),
                            },
                        })
                    ),
                    range: exp_range($i * 2 + 1, 1..12 + $op.len()),
                },
            )
        }
    }

    macro_rules! cases_for_operators {
        ($($i: expr => $op: expr,)*) => {
            vec![
                $(
                    case_for_higher_lhs_precedence!($i => $op),
                    case_for_higher_rhs_precedence!($i => $op),
                )*
            ]
        }
    }

    // There are no test cases for associativity,
    // because comparison operators are not associative.

    let cases = cases_for_operators![
        0 => "==",
        1 => "!=",
        2 => "<",
        3 => ">",
        4 => "<=",
        5 => ">=",
    ];

    evaluate(cases);
}

// Although comparison expressions are not associative, associating
// them explicitly via grouping/parentheses should still be allowed.
#[test]
fn valid_comparison_expression_noassoc() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "(a < b) < c",
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "<",
                        lhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: "<",
                                            lhs: Exp {
                                                kind: ExpKind::Identifier("a"),
                                                range: exp_range(0, 2..3),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Identifier("b"),
                                                range: exp_range(0, 6..7),
                                            },
                                        })
                                    ),
                                    range: exp_range(0, 2..7),
                                },
                            ]),
                            range: exp_range(0, 1..8),
                        },
                        rhs: Exp {
                            kind: ExpKind::Identifier("c"),
                            range: exp_range(0, 11..12),
                        },
                    })
                ),
                range: exp_range(0, 1..12),
            },
        ),
        (
            "42 > (43 > 44)",
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: ">",
                        lhs: Exp {
                            kind: ExpKind::Int("42"),
                            range: exp_range(1, 1..3),
                        },
                        rhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: ">",
                                            lhs: Exp {
                                                kind: ExpKind::Int("43"),
                                                range: exp_range(1, 7..9),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Int("44"),
                                                range: exp_range(1, 12..14),
                                            },
                                        })
                                    ),
                                    range: exp_range(1, 7..14),
                                },
                            ]),
                            range: exp_range(1, 6..15),
                        },
                    })
                ),
                range: exp_range(1, 1..15),
            },
        ),
        (
            "nil < (null <= true)",
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "<",
                        lhs: Exp {
                            kind: ExpKind::Nil,
                            range: exp_range(2, 1..4),
                        },
                        rhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: "<=",
                                            lhs: Exp {
                                                kind: ExpKind::Identifier("null"),
                                                range: exp_range(2, 8..12),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Bool("true"),
                                                range: exp_range(2, 16..20),
                                            },
                                        })
                                    ),
                                    range: exp_range(2, 8..20),
                                },
                            ]),
                            range: exp_range(2, 7..21),
                        },
                    })
                ),
                range: exp_range(2, 1..21),
            },
        ),
        (
            "(72 != 11.0) >= (768.0 == param512)",
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: ">=",
                        lhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: "!=",
                                            lhs: Exp {
                                                kind: ExpKind::Int("72"),
                                                range: exp_range(3, 2..4),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Float("11.0"),
                                                range: exp_range(3, 8..12),
                                            },
                                        })
                                    ),
                                    range: exp_range(3, 2..12),
                                },
                            ]),
                            range: exp_range(3, 1..13),
                        },
                        rhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: "==",
                                            lhs: Exp {
                                                kind: ExpKind::Float("768.0"),
                                                range: exp_range(3, 18..23),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Identifier("param512"),
                                                range: exp_range(3, 27..35),
                                            },
                                        })
                                    ),
                                    range: exp_range(3, 18..35),
                                },
                            ]),
                            range: exp_range(3, 17..36),
                        },
                    })
                ),
                range: exp_range(3, 1..36),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_and_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "p != 3 & q", // higher precedence on the LHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "&",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "!=",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("p"),
                                        range: exp_range(0, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("3"),
                                        range: exp_range(0, 6..7),
                                    },
                                })
                            ),
                            range: exp_range(0, 1..7),
                        },
                        rhs: Exp {
                            kind: ExpKind::Identifier("q"),
                            range: exp_range(0, 10..11),
                        },
                    })
                ),
                range: exp_range(0, 1..11),
            },
        ),
        (
            "r & s > 63", // higher precedence on the RHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "&",
                        lhs: Exp {
                            kind: ExpKind::Identifier("r"),
                            range: exp_range(1, 1..2),
                        },
                        rhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: ">",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("s"),
                                        range: exp_range(1, 5..6),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("63"),
                                        range: exp_range(1, 9..11),
                                    },
                                })
                            ),
                            range: exp_range(1, 5..11),
                        },
                    })
                ),
                range: exp_range(1, 1..11),
            },
        ),
        (
            "t >= u & v <= w", // higher precedence on both sides
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "&",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: ">=",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("t"),
                                        range: exp_range(2, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("u"),
                                        range: exp_range(2, 6..7),
                                    },
                                })
                            ),
                            range: exp_range(2, 1..7),
                        },
                        rhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "<=",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("v"),
                                        range: exp_range(2, 10..11),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("w"),
                                        range: exp_range(2, 15..16),
                                    },
                                })
                            ),
                            range: exp_range(2, 10..16),
                        },
                    })
                ),
                range: exp_range(2, 1..16),
            },
        ),
        (
            "true & false & file_not_found", // left associativity
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "&",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "&",
                                    lhs: Exp {
                                        kind: ExpKind::Bool("true"),
                                        range: exp_range(3, 1..5),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Bool("false"),
                                        range: exp_range(3, 8..13),
                                    },
                                })
                            ),
                            range: exp_range(3, 1..13),
                        },
                        rhs: Exp {
                            kind: ExpKind::Identifier("file_not_found"),
                            range: exp_range(3, 16..30),
                        },
                    })
                ),
                range: exp_range(3, 1..30),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_or_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "c & b | a", // higher precedence on the LHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "|",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "&",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("c"),
                                        range: exp_range(0, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("b"),
                                        range: exp_range(0, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(0, 1..6),
                        },
                        rhs: Exp {
                            kind: ExpKind::Identifier("a"),
                            range: exp_range(0, 9..10),
                        },
                    })
                ),
                range: exp_range(0, 1..10),
            },
        ),
        (
            "F | G & HH", // higher precedence on the RHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "|",
                        lhs: Exp {
                            kind: ExpKind::Identifier("F"),
                            range: exp_range(1, 1..2),
                        },
                        rhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "&",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("G"),
                                        range: exp_range(1, 5..6),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("HH"),
                                        range: exp_range(1, 9..11),
                                    },
                                })
                            ),
                            range: exp_range(1, 5..11),
                        },
                    })
                ),
                range: exp_range(1, 1..11),
            },
        ),
        (
            "i & j | k & 0", // higher precedence on both sides
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "|",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "&",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("i"),
                                        range: exp_range(2, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("j"),
                                        range: exp_range(2, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(2, 1..6),
                        },
                        rhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "&",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("k"),
                                        range: exp_range(2, 9..10),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("0"),
                                        range: exp_range(2, 13..14),
                                    },
                                })
                            ),
                            range: exp_range(2, 9..14),
                        },
                    })
                ),
                range: exp_range(2, 1..14),
            },
        ),
        (
            "false | true | null", // left associativity
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "|",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "|",
                                    lhs: Exp {
                                        kind: ExpKind::Bool("false"),
                                        range: exp_range(3, 1..6),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Bool("true"),
                                        range: exp_range(3, 9..13),
                                    },
                                })
                            ),
                            range: exp_range(3, 1..13),
                        },
                        rhs: Exp {
                            kind: ExpKind::Identifier("null"),
                            range: exp_range(3, 16..20),
                        },
                    })
                ),
                range: exp_range(3, 1..20),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_range_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "1 | 2..3", // higher precedence on LHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "..",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "|",
                                    lhs: Exp {
                                        kind: ExpKind::Int("1"),
                                        range: exp_range(0, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("2"),
                                        range: exp_range(0, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(0, 1..6),
                        },
                        rhs: Exp {
                            kind: ExpKind::Int("3"),
                            range: exp_range(0, 8..9),
                        },
                    })
                ),
                range: exp_range(0, 1..9),
            },
        ),
        (
            "x..y | z", // higher precedence on RHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "..",
                        lhs: Exp {
                            kind: ExpKind::Identifier("x"),
                            range: exp_range(1, 1..2),
                        },
                        rhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "|",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("y"),
                                        range: exp_range(1, 4..5),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("z"),
                                        range: exp_range(1, 8..9),
                                    },
                                })
                            ),
                            range: exp_range(1, 4..9),
                        },
                    })
                ),
                range: exp_range(1, 1..9),
            },
        ),
        (
            "Q | W...E", // higher precedence on LHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "...",
                        lhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "|",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("Q"),
                                        range: exp_range(2, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("W"),
                                        range: exp_range(2, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(2, 1..6),
                        },
                        rhs: Exp {
                            kind: ExpKind::Identifier("E"),
                            range: exp_range(2, 9..10),
                        },
                    })
                ),
                range: exp_range(2, 1..10),
            },
        ),
        (
            "R...T | Y", // higher precedence on RHS
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "...",
                        lhs: Exp {
                            kind: ExpKind::Identifier("R"),
                            range: exp_range(3, 1..2),
                        },
                        rhs: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "|",
                                    lhs: Exp {
                                        kind: ExpKind::Identifier("T"),
                                        range: exp_range(3, 5..6),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Identifier("Y"),
                                        range: exp_range(3, 9..10),
                                    },
                                })
                            ),
                            range: exp_range(3, 5..10),
                        },
                    })
                ),
                range: exp_range(3, 1..10),
            },
        ),
        (
            "x1..(x0..x2)", // non-associative operator, but explicit grouping still allowed
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "..",
                        lhs: Exp {
                            kind: ExpKind::Identifier("x1"),
                            range: exp_range(4, 1..3),
                        },
                        rhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: "..",
                                            lhs: Exp {
                                                kind: ExpKind::Identifier("x0"),
                                                range: exp_range(4, 6..8),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Identifier("x2"),
                                                range: exp_range(4, 10..12),
                                            },
                                        }),
                                    ),
                                    range: exp_range(4, 6..12),
                                },
                            ]),
                            range: exp_range(4, 5..13),
                        },
                    })
                ),
                range: exp_range(4, 1..13),
            },
        ),
        (
            "(i...j)...k",
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "...",
                        lhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: "...",
                                            lhs: Exp {
                                                kind: ExpKind::Identifier("i"),
                                                range: exp_range(5, 2..3),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Identifier("j"),
                                                range: exp_range(5, 6..7),
                                            },
                                        }),
                                    ),
                                    range: exp_range(5, 2..7),
                                },
                            ]),
                            range: exp_range(5, 1..8),
                        },
                        rhs: Exp {
                            kind: ExpKind::Identifier("k"),
                            range: exp_range(5, 11..12),
                        },
                    })
                ),
                range: exp_range(5, 1..12),
            },
        ),
        (
            "(2...8)..4",
            Exp {
                kind: ExpKind::BinaryOp(
                    Box::new(BinaryOp {
                        op: "..",
                        lhs: Exp {
                            kind: ExpKind::Tuple(vec![
                                Exp {
                                    kind: ExpKind::BinaryOp(
                                        Box::new(BinaryOp {
                                            op: "...",
                                            lhs: Exp {
                                                kind: ExpKind::Int("2"),
                                                range: exp_range(6, 2..3),
                                            },
                                            rhs: Exp {
                                                kind: ExpKind::Int("8"),
                                                range: exp_range(6, 6..7),
                                            },
                                        }),
                                    ),
                                    range: exp_range(6, 2..7),
                                },
                            ]),
                            range: exp_range(6, 1..8),
                        },
                        rhs: Exp {
                            kind: ExpKind::Int("4"),
                            range: exp_range(6, 10..11),
                        },
                    })
                ),
                range: exp_range(6, 1..11),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_conditional_expression() {
    let (exp_range, evaluate) = valid_expression_tester();

    let cases = vec![
        (
            "cond ? true : false", // simple case
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::Identifier("cond"),
                            range: exp_range(0, 1..5),
                        },
                        true_val: Some(Exp {
                            kind: ExpKind::Bool("true"),
                            range: exp_range(0, 8..12),
                        }),
                        false_val: Exp {
                            kind: ExpKind::Bool("false"),
                            range: exp_range(0, 15..20),
                        },
                    })
                ),
                range: exp_range(0, 1..20),
            },
        ),
        (
            "option ?: default", // nil coalescing, "Elvis"
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::Identifier("option"),
                            range: exp_range(1, 1..7),
                        },
                        true_val: None,
                        false_val: Exp {
                            kind: ExpKind::Identifier("default"),
                            range: exp_range(1, 11..18),
                        },
                    })
                ),
                range: exp_range(1, 1..18),
            },
        ),
        (
            "a ? b : c ? d : e", // right associativity
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::Identifier("a"),
                            range: exp_range(2, 1..2),
                        },
                        true_val: Some(Exp {
                            kind: ExpKind::Identifier("b"),
                            range: exp_range(2, 5..6),
                        }),
                        false_val: Exp {
                            kind: ExpKind::CondExp(
                                Box::new(CondExp {
                                    condition: Exp {
                                        kind: ExpKind::Identifier("c"),
                                        range: exp_range(2, 9..10),
                                    },
                                    true_val: Some(Exp {
                                        kind: ExpKind::Identifier("d"),
                                        range: exp_range(2, 13..14),
                                    }),
                                    false_val: Exp {
                                        kind: ExpKind::Identifier("e"),
                                        range: exp_range(2, 17..18),
                                    },
                                })
                            ),
                            range: exp_range(2, 9..18),
                        },
                    })
                ),
                range: exp_range(2, 1..18),
            },
        ),
        (
            "0...1 ? x : y", // higher precedence on the LHS
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "...",
                                    lhs: Exp {
                                        kind: ExpKind::Int("0"),
                                        range: exp_range(3, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("1"),
                                        range: exp_range(3, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(3, 1..6),
                        },
                        true_val: Some(Exp {
                            kind: ExpKind::Identifier("x"),
                            range: exp_range(3, 9..10),
                        }),
                        false_val: Exp {
                            kind: ExpKind::Identifier("y"),
                            range: exp_range(3, 13..14),
                        },
                    })
                ),
                range: exp_range(3, 1..14),
            },
        ),
        (
            "foo ? bar : 0b1..0xff", // higher precedence on the RHS
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::Identifier("foo"),
                            range: exp_range(4, 1..4),
                        },
                        true_val: Some(Exp {
                            kind: ExpKind::Identifier("bar"),
                            range: exp_range(4, 7..10),
                        }),
                        false_val: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "..",
                                    lhs: Exp {
                                        kind: ExpKind::Int("0b1"),
                                        range: exp_range(4, 13..16),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("0xff"),
                                        range: exp_range(4, 18..22),
                                    },
                                })
                            ),
                            range: exp_range(4, 13..22),
                        },
                    })
                ),
                range: exp_range(4, 1..22),
            },
        ),
        (
            "0...9 ?: 0.9", // higher precedence on the LHS
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "...",
                                    lhs: Exp {
                                        kind: ExpKind::Int("0"),
                                        range: exp_range(5, 1..2),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("9"),
                                        range: exp_range(5, 5..6),
                                    },
                                })
                            ),
                            range: exp_range(5, 1..6),
                        },
                        true_val: None,
                        false_val: Exp {
                            kind: ExpKind::Float("0.9"),
                            range: exp_range(5, 10..13),
                        },
                    })
                ),
                range: exp_range(5, 1..13),
            },
        ),
        (
            "verylong ?: 0xa..0b10", // higher precedence on the RHS
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::Identifier("verylong"),
                            range: exp_range(6, 1..9),
                        },
                        true_val: None,
                        false_val: Exp {
                            kind: ExpKind::BinaryOp(
                                Box::new(BinaryOp {
                                    op: "..",
                                    lhs: Exp {
                                        kind: ExpKind::Int("0xa"),
                                        range: exp_range(6, 13..16),
                                    },
                                    rhs: Exp {
                                        kind: ExpKind::Int("0b10"),
                                        range: exp_range(6, 18..22),
                                    },
                                })
                            ),
                            range: exp_range(6, 13..22),
                        },
                    })
                ),
                range: exp_range(6, 1..22),
            },
        ),
        (
            "foo ? bar ? baz : qux : lol", // right in the middle
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::Identifier("foo"),
                            range: exp_range(7, 1..4),
                        },
                        true_val: Some(Exp {
                            kind: ExpKind::CondExp(
                                Box::new(CondExp {
                                    condition: Exp {
                                        kind: ExpKind::Identifier("bar"),
                                        range: exp_range(7, 7..10),
                                    },
                                    true_val: Some(Exp {
                                        kind: ExpKind::Identifier("baz"),
                                        range: exp_range(7, 13..16),
                                    }),
                                    false_val: Exp {
                                        kind: ExpKind::Identifier("qux"),
                                        range: exp_range(7, 19..22),
                                    },
                                })
                            ),
                            range: exp_range(7, 7..22),
                        }),
                        false_val: Exp {
                            kind: ExpKind::Identifier("lol"),
                            range: exp_range(7, 25..28),
                        },
                    })
                ),
                range: exp_range(7, 1..28),
            },
        ),
        (
            "X ? Y ?: Z : W", // nil coalesce inside conditional
            Exp {
                kind: ExpKind::CondExp(
                    Box::new(CondExp {
                        condition: Exp {
                            kind: ExpKind::Identifier("X"),
                            range: exp_range(8, 1..2),
                        },
                        true_val: Some(Exp {
                            kind: ExpKind::CondExp(
                                Box::new(CondExp {
                                    condition: Exp {
                                        kind: ExpKind::Identifier("Y"),
                                        range: exp_range(8, 5..6),
                                    },
                                    true_val: None,
                                    false_val: Exp {
                                        kind: ExpKind::Identifier("Z"),
                                        range: exp_range(8, 10..11),
                                    },
                                })
                            ),
                            range: exp_range(8, 5..11),
                        }),
                        false_val: Exp {
                            kind: ExpKind::Identifier("W"),
                            range: exp_range(8, 14..15),
                        },
                    })
                ),
                range: exp_range(8, 1..15),
            },
        ),
    ];

    evaluate(cases);
}

//
// Invalid Expressions
//

#[test]
fn invalid_atomic_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { ) }",
            marker:  "         ^^",
            message: "Expected expression; found )",
        },
        InvalidTestCase {
            source:  "fn _() { & }",
            marker:  "         ^^",
            message: "Expected expression; found &",
        },
        InvalidTestCase {
            source:  "fn _() { * }",
            marker:  "         ^^",
            message: "Expected expression; found *",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_tuple_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { ( }",
            marker:  "           ^^",
            message: "Expected expression; found }",
        },
        InvalidTestCase {
            source:  "fn _() { (,) }",
            marker:  "          ^^",
            message: "Expected expression; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { (1, ((2)), 3, ,) }",
            marker:  "                       ^^  ",
            message: "Expected expression; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { (foo, bar } ",
            marker:  "                   ^^",
            message: "Expected , or ); found }",
        },
        InvalidTestCase {
            source:  "fn _() { (foo, bar, ",
            marker:  "                  ^^",
            message: "Expected ); found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { (nil true) }",
            marker:  "              ^___^",
            message: "Expected , or ); found true",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_array_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { [ }",
            marker:  "           ^^",
            message: "Expected expression; found }",
        },
        InvalidTestCase {
            source:  "fn _() { [,] }",
            marker:  "          ^^",
            message: "Expected expression; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { [[a], b, [c], ,] }",
            marker:  "                       ^^  ",
            message: "Expected expression; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { [baz, qux } ",
            marker:  "                   ^^",
            message: "Expected , or ]; found }",
        },
        InvalidTestCase {
            source:  "fn _() { [qux, baz, ",
            marker:  "                  ^^",
            message: "Expected ]; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { [1 2] }",
            marker:  "            ^^",
            message: "Expected , or ]; found 2",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_block_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() {",
            marker:  "       ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { 1337 ",
            marker:  "         ^___^",
            message: "Expected ; or }; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { 42; ",
            marker:  "           ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { multiple; expressions; ",
            marker:  "                              ^^",
            message: "Expected }; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { stuff more_stuff }",
            marker:  "               ^_________^",
            message: "Expected ; or }; found more_stuff",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_if_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { if ",
            marker:  "         ^_^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { if foo then",
            marker:  "                ^___^",
            message: "Expected {; found then",
        },
        InvalidTestCase {
            source:  "fn _() { if condition ",
            marker:  "            ^________^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { (if condition { ) }",
            marker:  "                         ^^ ",
            message: "Expected expression; found )",
        },
        InvalidTestCase {
            source:  "fn _() { if stuff { } else }",
            marker:  "                           ^^",
            message: "Expected if or block after else; found }",
        },
        InvalidTestCase {
            source:  "fn _() { if foo { } else if",
            marker:  "                         ^_^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { if goo { } else if wat",
            marker:  "                            ^__^",
            message: "Expected {; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { if whoo { } else if other {",
            marker:  "                                   ^^",
            message: "Expected }; found end of input",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_function_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { |",
            marker:  "         ^^ ",
            message: "Expected |; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { |as",
            marker:  "          ^_^ ",
            message: "Expected identifier; found as",
        },
        InvalidTestCase {
            source:  "fn _() { |arg_name",
            marker:  "          ^_______^",
            message: "Expected , or |; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { |arg_name,,",
            marker:  "                   ^^",
            message: "Expected identifier; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { |arg_name:",
            marker:  "                  ^^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { |arg_name: ,",
            marker:  "                    ^^",
            message: "Expected a type; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { |x: T",
            marker:  "             ^^",
            message: "Expected , or |; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { |y: U, ",
            marker:  "              ^^",
            message: "Expected |; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { |foo: Bar|",
            marker:  "                  ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { |bar: Foo| != } blah-blah",
            marker:  "                    ^_^           ",
            message: "Expected expression; found !=",
        },
        InvalidTestCase {
            source:  "fn _() { |x: Y| * }",
            marker:  "                ^^",
            message: "Expected expression; found *",
        },
        InvalidTestCase {
            source:  "fn _() { |value| ->",
            marker:  "                 ^_^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { |_| -> [&Wowza?] }",
            marker:  "                          ^^",
            message: "Expected {; found }",
        },
        InvalidTestCase {
            source:  "fn _() { |_| -> [&Huzzah?] expr + more }",
            marker:  "                           ^___^        ",
            message: "Expected {; found expr",
        },
        InvalidTestCase {
            source:  "fn _() { |a| -> T { }",
            marker:  "                    ^^",
            message: "Expected ; or }; found end of input",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_subscript_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { something[",
            marker:  "                  ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { base[index",
            marker:  "              ^____^",
            message: "Expected ]; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { base[index }",
            marker:  "                    ^^",
            message: "Expected ]; found }",
        },
        InvalidTestCase {
            source:  "fn _() { v[1, 2] }",
            marker:  "            ^^    ",
            message: "Expected ]; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { v[1; 2] }",
            marker:  "            ^^    ",
            message: "Expected ]; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { v[1 2] }",
            marker:  "             ^^  ",
            message: "Expected ]; found 2",
        },
        InvalidTestCase {
            source:  "fn _() { [[multi]][[] }",
            marker:  "                      ^^",
            message: "Expected ]; found }",
        },
        InvalidTestCase {
            source:  "fn _() { [[moar]][[]][] }",
            marker:  "                      ^^",
            message: "Expected expression; found ]",
        },
        InvalidTestCase {
            source:  "fn _() { [[[even_moar]]][[[]]][",
            marker:  "                              ^^",
            message: "Expected expression; found end of input",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_call_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { cally(",
            marker:  "              ^^",
            message: "Expected ); found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { fun(,) }",
            marker:  "             ^^",
            message: "Expected expression; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { more_fun(nil, nil,,) }",
            marker:  "                           ^^",
            message: "Expected expression; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { even_more_fun(expr } ",
            marker:  "                            ^^",
            message: "Expected , or ); found }",
        },
        InvalidTestCase {
            source:  "fn _() { kitty(doge(sheep()) }",
            marker:  "                             ^^",
            message: "Expected , or ); found }",
        },
        InvalidTestCase {
            source:  "fn _() { _(_(_(nested)),,) }",
            marker:  "                        ^^  ",
            message: "Expected expression; found ,",
        },
        InvalidTestCase {
            source:  "fn _() { _(_(_(more(nested,)),),",
            marker:  "                               ^^",
            message: "Expected ); found end of input",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_member_access_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { keyword.as }",
            marker:  "                 ^_^ ",
            message: "Expected identifier; found as",
        },
        InvalidTestCase {
            source:  "fn _() { keyword.multi.class }",
            marker:  "                       ^____^ ",
            message: "Expected identifier; found class",
        },
        InvalidTestCase {
            source:  "fn _() { infix.enum.between }",
            marker:  "               ^___^         ",
            message: "Expected identifier; found enum",
        },
        InvalidTestCase {
            source:  "fn _() { babble. }",
            marker:  "                 ^^",
            message: "Expected identifier; found }",
        },
        InvalidTestCase {
            source:  "fn _() { babble.bubble.() }",
            marker:  "                       ^^",
            message: "Expected identifier; found (",
        },
        InvalidTestCase {
            source:  "fn _() { one.",
            marker:  "            ^^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { unicorns.ponies.",
            marker:  "                        ^^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { keyword::nil }",
            marker:  "                  ^__^ ",
            message: "Expected identifier; found nil",
        },
        InvalidTestCase {
            source:  "fn _() { kwd::several::struct }",
            marker:  "                       ^_____^ ",
            message: "Expected identifier; found struct",
        },
        InvalidTestCase {
            source:  "fn _() { between::impl::infix }",
            marker:  "                  ^___^        ",
            message: "Expected identifier; found impl",
        },
        InvalidTestCase {
            source:  "fn _() { [ babble:: ] }",
            marker:  "                    ^^",
            message: "Expected identifier; found ]",
        },
        InvalidTestCase {
            source:  "fn _() { bubble::babble::[] }",
            marker:  "                         ^^  ",
            message: "Expected identifier; found [",
        },
        InvalidTestCase {
            source:  "fn _() { single:: ",
            marker:  "               ^_^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { pink::fluffy::rainbows::",
            marker:  "                               ^_^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { mixed.member::qual.",
            marker:  "                           ^^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { other::way.around::",
            marker:  "                          ^_^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { a::b.c::d::e.false }",
            marker:  "                      ^____^",
            message: "Expected identifier; found false",
        },
        InvalidTestCase {
            source:  "fn _() { x.y.z::u::v::w.- }",
            marker:  "                        ^^",
            message: "Expected identifier; found -",
        },
        InvalidTestCase {
            source:  "fn _() { foo::GOO.Boo::+ }",
            marker:  "                       ^^",
            message: "Expected identifier; found +",
        },
        InvalidTestCase {
            source:  "fn _() { numbers.0.not.allowed }",
            marker:  "                 ^^",
            message: "Expected identifier; found 0",
        },
        InvalidTestCase {
            source:  "fn _() { neither::here::99.87 }",
            marker:  "                        ^____^",
            message: "Expected identifier; found 99.87",
        },
        InvalidTestCase {
            source:  "fn _() { nor.1337::there }",
            marker:  "             ^___^",
            message: "Expected identifier; found 1337",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_prefix_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { + }",
            marker:  "           ^^",
            message: "Expected expression; found }",
        },
        InvalidTestCase {
            source:  "fn _() { -",
            marker:  "         ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { ~+--+-~+-",
            marker:  "                 ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { (((-++-+~-+-~))) }",
            marker:  "                      ^^   ",
            message: "Expected expression; found )",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_cast_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { 12345 as 98765 }",
            marker:  "                  ^____^",
            message: "Expected a type; found 98765",
        },
        InvalidTestCase {
            source:  "fn _() { nil as ~? }",
            marker:  "                ^^",
            message: "Expected a type; found ~",
        },
        InvalidTestCase {
            source:  "fn _() { (1 + 2 / 3 * 4) as }",
            marker:  "                            ^^",
            message: "Expected a type; found }",
        },
        InvalidTestCase {
            source:  "fn _() { [complex][0][expression] as Type1 as Type2 as",
            marker:  "                                                    ^_^",
            message: "Expected a type; found end of input",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_binary_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { 1 + 2 - 3 * 4 / 5 % 6 & }",
            marker:  "                                 ^^",
            message: "Expected expression; found }",
        },
        InvalidTestCase {
            source:  "fn _() { and | or | xor |",
            marker:  "                        ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { yes | no | }",
            marker:  "                    ^^",
            message: "Expected expression; found }",
        },
        InvalidTestCase {
            source:  "fn _() { wat || ugh }", // 2nd '|' is start of a closure
            marker:  "                    ^^",
            message: "Expected , or |; found }",
        },
        InvalidTestCase {
            source:  "fn _() { and && nope }",
            marker:  "              ^^",
            message: "Expected expression; found &",
        },
        InvalidTestCase {
            source:  "fn _() { aaa == bbb == zzz }",
            marker:  "                    ^_^",
            message: "Binary operator == is not associative",
        },
        InvalidTestCase {
            source:  "fn _() { wat <= hey == nope }",
            marker:  "                    ^_^",
            message: "Binary operator <= is not associative",
        },
        InvalidTestCase {
            source:  "fn _() { 42 + 43 > 59 / 58 < -1 - +2 }",
            marker:  "                           ^^",
            message: "Binary operator > is not associative",
        },
        InvalidTestCase {
            source:  "fn _() { (0 >= -1) != [funky > ops] == ((() > [])) }",
            marker:  "                                    ^_^             ",
            message: "Binary operator != is not associative",
        },
        InvalidTestCase {
            source:  "fn _() { begin..middle..end }",
            marker:  "                      ^_^    ",
            message: "Binary operator .. is not associative",
        },
        InvalidTestCase {
            source:  "fn _() { start...continue...stop }",
            marker:  "                         ^__^    ",
            message: "Binary operator ... is not associative",
        },
        InvalidTestCase {
            source:  "fn _() { -1..0...1 }",
            marker:  "              ^__^",
            message: "Binary operator .. is not associative",
        },
        InvalidTestCase {
            source:  "fn _() { -1.0...0.0..+1 }",
            marker:  "                   ^_^   ",
            message: "Binary operator ... is not associative",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_conditional_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { cond ?",
            marker:  "              ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { cond ? iftrue",
            marker:  "                ^_____^",
            message: "Expected :; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { cond ? iftrue :",
            marker:  "                       ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { nullable ?: ",
            marker:  "                   ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { boolean ? value : ] }",
            marker:  "                           ^^ ",
            message: "Expected expression; found ]",
        },
        InvalidTestCase {
            source:  "fn _() { maybe ?: / }",
            marker:  "                  ^^",
            message: "Expected expression; found /",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_variable_declaration() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { let let = 0; }",
            marker:  "             ^__^",
            message: "Expected identifier; found let",
        },
        InvalidTestCase {
            source:  "fn _() { let else = dangling; }",
            marker:  "             ^___^",
            message: "Expected identifier; found else",
        },
        InvalidTestCase {
            source:  "fn _() { let",
            marker:  "         ^__^",
            message: "Expected identifier; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { let _ {",
            marker:  "               ^^",
            message: "Expected =; found {",
        },
        InvalidTestCase {
            source:  "fn _() { let _ ",
            marker:  "             ^^",
            message: "Expected =; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { let var:",
            marker:  "                ^^",
            message: "Expected a type; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { let var: ]",
            marker:  "                  ^^",
            message: "Expected a type; found ]",
        },
        InvalidTestCase {
            source:  "fn _() { let var: Whoo",
            marker:  "                  ^___^",
            message: "Expected =; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { let var: Whoo; }",
            marker:  "                      ^^",
            message: "Expected =; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { let var: Whoo }",
            marker:  "                       ^^",
            message: "Expected =; found }",
        },
        InvalidTestCase {
            source:  "fn _() { let var: Whoo-hoo }",
            marker:  "                      ^^",
            message: "Expected =; found -",
        },
        InvalidTestCase {
            source:  "fn _() { let _x: Whatever = }",
            marker:  "                            ^^",
            message: "Expected expression; found }",
        },
        InvalidTestCase {
            source:  "fn _() { let _x: Whatever =",
            marker:  "                          ^^",
            message: "Expected expression; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { let _y: [Stuff?] = nope }",
            marker:  "                                 ^^",
            message: "Expected ;; found }",
        },
        InvalidTestCase {
            source:  "fn _() { let _y: [Stuff?] = still_not",
            marker:  "                            ^________^",
            message: "Expected ;; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { let typeless = value",
            marker:  "                        ^____^",
            message: "Expected ;; found end of input",
        },
        InvalidTestCase {
            source:  "fn _() { let typeless2 = value2 }",
            marker:  "                                ^^",
            message: "Expected ;; found }",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn semicolon_is_not_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { let var = ; }",
            marker:  "                   ^^ ",
            message: "Expected expression; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { if ; {} }",
            marker:  "            ^^    ",
            message: "Expected expression; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { 1338 * ; }",
            marker:  "                ^^ ",
            message: "Expected expression; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { 1339.0..; }",
            marker:  "                 ^^ ",
            message: "Expected expression; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { +-~; }",
            marker:  "            ^^ ",
            message: "Expected expression; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { ((),;,()) }",
            marker:  "             ^^",
            message: "Expected expression; found ;",
        },
        InvalidTestCase {
            source:  "fn _() { [[[;]]] }",
            marker:  "            ^^    ",
            message: "Expected expression; found ;",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn let_is_not_expression() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() { let one = let two = three; }",
            marker:  "                   ^__^",
            message: "Expected expression; found let",
        },
        InvalidTestCase {
            source:  "fn _() { if let one = 01234567 { stuff(); } }",
            marker:  "            ^__^",
            message: "Expected expression; found let",
        },
        InvalidTestCase {
            source:  "fn _() { [let _ = _;] }",
            marker:  "          ^__^         ",
            message: "Expected expression; found let",
        },
        InvalidTestCase {
            source:  "fn _() { (((let does_not_work = 0))) }",
            marker:  "            ^__^                      ",
            message: "Expected expression; found let",
        },
    ];

    test_invalid_cases(test_cases);
}

//
// Valid Types
//

#[test]
fn valid_named_type() {
    let (ty_range, evaluate) = valid_type_tester();

    let cases = vec![
        (
            "MyAwesomeType",
            Ty {
                kind: TyKind::Named("MyAwesomeType"),
                range: ty_range(0, 1..14),
            },
        ),
        (
            "_",
            Ty {
                kind: TyKind::Named("_"),
                range: ty_range(1, 1..2),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_array_type() {
    let (ty_range, evaluate) = valid_type_tester();

    let cases = vec![
        (
            "[Element]",
            Ty {
                kind: TyKind::Array(
                    Box::new(Ty {
                        kind: TyKind::Named("Element"),
                        range: ty_range(0, 2..9),
                    })
                ),
                range: ty_range(0, 1..10),
            },
        ),
        (
            "[[Nested]]",
            Ty {
                kind: TyKind::Array(
                    Box::new(Ty {
                        kind: TyKind::Array(
                            Box::new(Ty {
                                kind: TyKind::Named("Nested"),
                                range: ty_range(1, 3..9),
                            })
                        ),
                        range: ty_range(1, 2..10),
                    })
                ),
                range: ty_range(1, 1..11),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_tuple_type() {
    let (ty_range, evaluate) = valid_type_tester();

    let cases = vec![
        (
            "()",
            Ty {
                kind: TyKind::Tuple(vec![]),
                range: ty_range(0, 1..3),
            },
        ),
        (
            "(())",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: ty_range(1, 2..4),
                    },
                ]),
                range: ty_range(1, 1..5),
            },
        ),
        (
            "((),)",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: ty_range(2, 2..4),
                    },
                ]),
                range: ty_range(2, 1..6),
            },
        ),
        (
            "((),())",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: ty_range(3, 2..4),
                    },
                    Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: ty_range(3, 5..7),
                    },
                ]),
                range: ty_range(3, 1..8),
            },
        ),
        (
            "((),(),)",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: ty_range(4, 2..4),
                    },
                    Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: ty_range(4, 5..7),
                    },
                ]),
                range: ty_range(4, 1..9),
            },
        ),
        (
            "(T)",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Named("T"),
                        range: ty_range(5, 2..3),
                    },
                ]),
                range: ty_range(5, 1..4),
            },
        ),
        (
            "(TT , )",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Named("TT"),
                        range: ty_range(6, 2..4),
                    },
                ]),
                range: ty_range(6, 1..8),
            },
        ),
        (
            "(Ultra, Violet)",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Named("Ultra"),
                        range: ty_range(7, 2..7),
                    },
                    Ty {
                        kind: TyKind::Named("Violet"),
                        range: ty_range(7, 9..15),
                    },
                ]),
                range: ty_range(7, 1..16),
            },
        ),
        (
            "(Woo , Hoo,)",
            Ty {
                kind: TyKind::Tuple(vec![
                    Ty {
                        kind: TyKind::Named("Woo"),
                        range: ty_range(8, 2..5),
                    },
                    Ty {
                        kind: TyKind::Named("Hoo"),
                        range: ty_range(8, 8..11),
                    },
                ]),
                range: ty_range(8, 1..13),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_prefix_type() {
    let (ty_range, evaluate) = valid_type_tester();

    let cases = vec![
        (
            "&Pointee",
            Ty {
                kind: TyKind::Pointer(
                    Box::new(Ty {
                        kind: TyKind::Named("Pointee"),
                        range: ty_range(0, 2..9),
                    })
                ),
                range: ty_range(0, 1..9),
            },
        ),
        (
            "&&Nestie",
            Ty {
                kind: TyKind::Pointer(
                    Box::new(Ty {
                        kind: TyKind::Pointer(
                            Box::new(Ty {
                                kind: TyKind::Named("Nestie"),
                                range: ty_range(1, 3..9),
                            })
                        ),
                        range: ty_range(1, 2..9),
                    })
                ),
                range: ty_range(1, 1..9),
            },
        ),
        (
            "&[PointerToCompound]",
            Ty {
                kind: TyKind::Pointer(
                    Box::new(Ty {
                        kind: TyKind::Array(
                            Box::new(Ty {
                                kind: TyKind::Named("PointerToCompound"),
                                range: ty_range(2, 3..20),
                            })
                        ),
                        range: ty_range(2, 2..21),
                    })
                ),
                range: ty_range(2, 1..21),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_postfix_type() {
    let (ty_range, evaluate) = valid_type_tester();

    let cases = vec![
        (
            "Simple?",
            Ty {
                kind: TyKind::Optional(
                    Box::new(Ty {
                        kind: TyKind::Named("Simple"),
                        range: ty_range(0, 1..7),
                    })
                ),
                range: ty_range(0, 1..8),
            },
        ),
        (
            "(Compound)?",
            Ty {
                kind: TyKind::Optional(
                    Box::new(Ty {
                        kind: TyKind::Tuple(vec![
                            Ty {
                                kind: TyKind::Named("Compound"),
                                range: ty_range(1, 2..10),
                            },
                        ]),
                        range: ty_range(1, 1..11),
                    })
                ),
                range: ty_range(1, 1..12),
            },
        ),
        (
            "&Precedence?",
            Ty {
                kind: TyKind::Optional(
                    Box::new(Ty {
                        kind: TyKind::Pointer(
                            Box::new(Ty {
                                kind: TyKind::Named("Precedence"),
                                range: ty_range(2, 2..12),
                            })
                        ),
                        range: ty_range(2, 1..12),
                    })
                ),
                range: ty_range(2, 1..13),
            },
        ),
        (
            "Double??",
            Ty {
                kind: TyKind::Optional(
                    Box::new(Ty {
                        kind: TyKind::Optional(
                            Box::new(Ty {
                                kind: TyKind::Named("Double"),
                                range: ty_range(3, 1..7),
                            })
                        ),
                        range: ty_range(3, 1..8),
                    })
                ),
                range: ty_range(3, 1..9),
            },
        ),
    ];

    evaluate(cases);
}

#[test]
fn valid_function_type() {
    let (ty_range, evaluate) = valid_type_tester();

    let cases = vec![
        (
            "() -> ()",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Tuple(vec![]),
                        range: ty_range(0, 7..9),
                    }),
                }),
                range: ty_range(0, 1..9),
            },
        ),
        (
            "(Arg) -> Ret",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Named("Arg"),
                            range: ty_range(1, 2..5),
                        },
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Named("Ret"),
                        range: ty_range(1, 10..13),
                    }),
                }),
                range: ty_range(1, 1..13),
            },
        ),
        (
            "(Arg1, Arg2) -> Ret",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Named("Arg1"),
                            range: ty_range(2, 2..6),
                        },
                        Ty {
                            kind: TyKind::Named("Arg2"),
                            range: ty_range(2, 8..12),
                        },
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Named("Ret"),
                        range: ty_range(2, 17..20),
                    }),
                }),
                range: ty_range(2, 1..20),
            },
        ),
        (
            "(T, U,) -> V",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Named("T"),
                            range: ty_range(3, 2..3),
                        },
                        Ty {
                            kind: TyKind::Named("U"),
                            range: ty_range(3, 5..6),
                        },
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Named("V"),
                        range: ty_range(3, 12..13),
                    }),
                }),
                range: ty_range(3, 1..13),
            },
        ),
        (
            "SingleArg -> SingleRet",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Named("SingleArg"),
                            range: ty_range(4, 1..10),
                        },
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Named("SingleRet"),
                        range: ty_range(4, 14..23),
                    }),
                }),
                range: ty_range(4, 1..23),
            },
        ),
        (
            "OneArg -> (Many, Returns)",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Named("OneArg"),
                            range: ty_range(5, 1..7),
                        },
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Tuple(vec![
                            Ty {
                                kind: TyKind::Named("Many"),
                                range: ty_range(5, 12..16),
                            },
                            Ty {
                                kind: TyKind::Named("Returns"),
                                range: ty_range(5, 18..25),
                            },
                        ]),
                        range: ty_range(5, 11..26),
                    }),
                }),
                range: ty_range(5, 1..26),
            },
        ),
        (
            "&LhsHigherPrecedence -> Foo",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Pointer(
                                Box::new(Ty {
                                    kind: TyKind::Named("LhsHigherPrecedence"),
                                    range: ty_range(6, 2..21),
                                })
                            ),
                            range: ty_range(6, 1..21),
                        },
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Named("Foo"),
                        range: ty_range(6, 25..28),
                    }),
                }),
                range: ty_range(6, 1..28),
            },
        ),
        (
            "Bar -> RhsHigherPrecedence?",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Named("Bar"),
                            range: ty_range(7, 1..4),
                        }
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Optional(
                            Box::new(Ty {
                                kind: TyKind::Named("RhsHigherPrecedence"),
                                range: ty_range(7, 8..27),
                            })
                        ),
                        range: ty_range(7, 8..28),
                    }),
                }),
                range: ty_range(7, 1..28),
            },
        ),
        (
            "Right -> Associative -> Arrows",
            Ty {
                kind: TyKind::Function(FunctionTy {
                    arg_types: vec![
                        Ty {
                            kind: TyKind::Named("Right"),
                            range: ty_range(8, 1..6),
                        },
                    ],
                    ret_type: Box::new(Ty {
                        kind: TyKind::Function(FunctionTy {
                            arg_types: vec![
                                Ty {
                                    kind: TyKind::Named("Associative"),
                                    range: ty_range(8, 10..21),
                                },
                            ],
                            ret_type: Box::new(Ty {
                                kind: TyKind::Named("Arrows"),
                                range: ty_range(8, 25..31),
                            }),
                        }),
                        range: ty_range(8, 10..31),
                    }),
                }),
                range: ty_range(8, 1..31),
            },
        ),
    ];

    evaluate(cases);
}

//
// Invalid Types
//

#[test]
fn invalid_atomic_type() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _(_: if) {}",
            marker:  "        ^_^",
            message: "Expected a type; found if",
        },
        InvalidTestCase {
            source:  "fn _(_: 8.3) {}",
            marker:  "        ^__^",
            message: "Expected a type; found 8.3",
        },
        InvalidTestCase {
            source:  "fn _() -> nil {}",
            marker:  "          ^__^",
            message: "Expected a type; found nil",
        },
        InvalidTestCase {
            source:  r#"fn _() -> "stringy" {}"#,
            marker:  r#"          ^________^"#,
            message: r#"Expected a type; found "stringy""#,
        },
        InvalidTestCase {
            source:  "fn _(_: %) {}",
            marker:  "        ^^",
            message: "Expected a type; found %",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_tuple_type() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _() -> (SomeType {}",
            marker:  "                    ^^",
            message: "Expected , or ); found {",
        },
        InvalidTestCase {
            source:  "fn _(_: (Item,,)) {}",
            marker:  "              ^^",
            message: "Expected a type; found ,",
        },
        InvalidTestCase {
            source:  "fn _(_: (A; B)) {}",
            marker:  "          ^^",
            message: "Expected , or ); found ;",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_array_type() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _(_: []) {}",
            marker:  "         ^^",
            message: "Expected a type; found ]",
        },
        InvalidTestCase {
            source:  "fn _(_: [Type,]) {}",
            marker:  "             ^^",
            message: "Expected ]; found ,",
        },
        InvalidTestCase {
            source:  "fn _(_: [nil]) {}",
            marker:  "         ^__^",
            message: "Expected a type; found nil",
        },
        InvalidTestCase {
            source:  "fn _(_: [Incomplete) {}",
            marker:  "                   ^^",
            message: "Expected ]; found )",
        },
        InvalidTestCase {
            source:  "fn _(_: [[Nested]) {}",
            marker:  "                 ^^",
            message: "Expected ]; found )",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_prefix_type() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _(_: &) {}",
            marker:  "         ^^",
            message: "Expected a type; found )",
        },
        InvalidTestCase {
            source:  "fn _(_: &false) {}",
            marker:  "         ^____^",
            message: "Expected a type; found false",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_postfix_type() {
    // Most of these cases chiefly test if the postfix parser
    // propagates errors in subexpressions up the call chain.
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _(_: ?) {}",
            marker:  "        ^^",
            message: "Expected a type; found ?",
        },
        InvalidTestCase {
            source:  "fn _(_: true?) {}",
            marker:  "        ^___^",
            message: "Expected a type; found true",
        },
        InvalidTestCase {
            source:  "fn _(_: (Foo,?)) {}",
            marker:  "             ^^",
            message: "Expected a type; found ?",
        },
        InvalidTestCase {
            source:  "fn _(_: []?) {}",
            marker:  "         ^^",
            message: "Expected a type; found ]",
        },
    ];

    test_invalid_cases(test_cases);
}

#[test]
fn invalid_function_type() {
    let test_cases: &[_] = &[
        InvalidTestCase {
            source:  "fn _(_: Missing -> ) {}",
            marker:  "                   ^^",
            message: "Expected a type; found )",
        },
        InvalidTestCase {
            source:  "fn _(_: Still -> Missing -> ) {}",
            marker:  "                            ^^",
            message: "Expected a type; found )",
        },
        InvalidTestCase {
            source:  "fn _(_: Keyword -> as) {}",
            marker:  "                   ^_^",
            message: "Expected a type; found as",
        },
        InvalidTestCase {
            source:  "fn _(_: -> T) {}",
            marker:  "        ^_^",
            message: "Expected a type; found ->",
        },
        InvalidTestCase {
            source:  "fn _(_: U -> [V) {}",
            marker:  "               ^^",
            message: "Expected ]; found )",
        },
        InvalidTestCase {
            source:  "fn _(_: (Q,,) -> R) {}",
            marker:  "           ^^",
            message: "Expected a type; found ,",
        },
    ];

    test_invalid_cases(test_cases);
}

// Test parsing if, match and block {} constructs in statement position
// (no separating semicolon required).
// Also test that multi-line range generation is working correctly.
// (multi-source is already being tested by almost all cases anyway.)
#[test]
fn statement_position_and_multiline_range() {
    type LineCol = (usize, usize);

    fn range((from_line, from_col): LineCol, (to_line, to_col): LineCol) -> Range {
        Range {
            start: Location { src_idx: 0, line: from_line, column: from_col },
            end:   Location { src_idx: 0, line: to_line,   column: to_col   },
        }
    }

    let sources = [r#"
        fn stmt_multi_line() {
            {
            }
            {
            }
            if quacky {}
            if "mmm" {}
        }
    "#];

    let item = Item::FuncDef(Function {
        range: range((2, 9), (9, 10)),
        name: Some("stmt_multi_line"),
        arguments: vec![],
        ret_type: None,
        body: Exp {
            kind: ExpKind::Block(vec![
                Exp {
                    kind: ExpKind::Block(vec![]),
                    range: range((3, 13), (4, 14)),
                },
                Exp {
                    kind: ExpKind::Block(vec![]),
                    range: range((5, 13), (6, 14)),
                },
                Exp {
                    kind: ExpKind::If(
                        Box::new(If {
                            condition: Exp {
                                kind: ExpKind::Identifier("quacky"),
                                range: range((7, 16), (7, 22)),
                            },
                            then_arm: Exp {
                                kind: ExpKind::Block(vec![]),
                                range: range((7, 23), (7, 25)),
                            },
                            else_arm: None,
                        })
                    ),
                    range: range((7, 13), (7, 25)),
                },
                Exp {
                    kind: ExpKind::If(
                        Box::new(If {
                            condition: Exp {
                                kind: ExpKind::String(r#""mmm""#),
                                range: range((8, 16), (8, 21)),
                            },
                            then_arm: Exp {
                                kind: ExpKind::Block(vec![]),
                                range: range((8, 22), (8, 24)),
                            },
                            else_arm: None,
                        })
                    ),
                    range: range((8, 13), (8, 24)),
                },
            ]),
            range: range((2, 30), (9, 10)),
        },
    });

    let expected_ast = Prog { items: vec![item] };
    let tokens = lex_filter_ws_comment(&sources);
    let actual_ast = parse_valid(&tokens);

    assert_eq!(actual_ast, expected_ast);
}
