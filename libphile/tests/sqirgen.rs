//
// tests/sqirgen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 01/11/2017
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
extern crate regex;
extern crate phile;

mod common;

use common::*;
use phile::error::{ Error, Result };
use phile::util::Range;
use phile::lexer::{ lex, TokenKind };
use phile::parser::parse;
use phile::sqir::*;
use phile::sqirgen::generate_sqir;

fn try_generate_sqir(source: &str) -> Result<Sqir> {
    let sources = [source];
    let mut tokens = lex(&sources).unwrap();

    tokens.retain(|token| match token.kind {
        TokenKind::Whitespace => false,
        TokenKind::Comment => false,
        _ => true,
    });

    let ast = parse(&tokens).unwrap();

    generate_sqir(&ast)
}

fn sqir_for_valid_source(source: &str) -> Sqir {
    try_generate_sqir(source).unwrap()
}

fn sema_error_for_invalid_source(source: &str) -> (String, Range) {
    let error = try_generate_sqir(source).unwrap_err();

    match error {
        Error::Semantic { message, range } => (message, range),
        _ => panic!("SqirGen returned a non-semantic error: {}", error),
    }
}

#[test]
fn empty_source() {
    let sqir = sqir_for_valid_source("");

    assert!(sqir.decimal_types.is_empty());
    assert!(sqir.optional_types.is_empty());
    assert!(sqir.pointer_types.is_empty());
    assert!(sqir.array_types.is_empty());
    assert!(sqir.tuple_types.is_empty());
    assert!(sqir.function_types.is_empty());
    assert!(sqir.relations.is_empty());
    assert!(sqir.globals.is_empty());

    for (name, ty) in sqir.named_types {
        match *ty.borrow().unwrap() {
            Type::Enum(_) | Type::Struct(_) | Type::Class(_) => panic!(
                "User-defined type {} generated from empty source?!",
                name,
            ),
            _ => {},
        }
    }
}

#[test]
fn duplicate_user_defined_type() {
    let cases: &[_] = &[
        InvalidTestCase {
            source:  "struct Foo {} struct Foo {}",
            marker:  "              ^____________^",
            message: "Redefinition of 'Foo'",
        },
        InvalidTestCase {
            source:  "class  Bar {} class  Bar {} ",
            marker:  "              ^____________^",
            message: "Redefinition of 'Bar'",
        },
        InvalidTestCase {
            source:  "enum   Qux {} enum   Qux {}",
            marker:  "              ^____________^",
            message: "Redefinition of 'Qux'",
        },
        InvalidTestCase {
            source:  "struct User {} class User {}",
            marker:  "               ^____________^",
            message: "Redefinition of 'User'",
        },
        InvalidTestCase {
            source:  "enum Post {} class Post {}",
            marker:  "             ^____________^",
            message: "Redefinition of 'Post'",
        },
        InvalidTestCase {
            source:  "struct Attachment {} enum Attachment {} ",
            marker:  "                     ^_________________^",
            message: "Redefinition of 'Attachment'",
        },
    ];

    for case in cases {
        let (actual_message, actual_range) = sema_error_for_invalid_source(case.source);
        let expected_message = case.message;
        let expected_range = case.error_range();

        assert_eq!(actual_message, expected_message);
        assert_eq!(actual_range, expected_range);
    }
}
