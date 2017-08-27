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

use phile::ast;
use phile::parser;


#[test]
fn empty_source() {
    match parser::parse(&[]) {
        Ok(ast) => assert!(ast.is_empty()),
        Err(err) => panic!("Empty source erroneously rejected: {}", err),
    }
}
