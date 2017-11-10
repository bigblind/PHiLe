//
// tests/sqirgen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 01/11/2017
//

#![cfg(test)]
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

extern crate phile;

use phile::error::Error;
use phile::util::Range;
use phile::{ lexer, parser, sqirgen };
use phile::sqir::*;


fn sqir_for_valid_source(source: &str) -> Sqir {
    let sources = [source];
    let tokens = lexer::lex(&sources).unwrap();
    let ast = parser::parse(&tokens).unwrap();

    sqirgen::generate_sqir(&ast).unwrap()
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
