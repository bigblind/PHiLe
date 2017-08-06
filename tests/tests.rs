//
// tests/tests.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 30/07/2017
//

#![cfg(test)]
#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications)]

#[macro_use]
extern crate quickcheck;
#[macro_use]
extern crate lazy_static;
extern crate unicode_xid;
extern crate phile;
extern crate regex;

pub mod lexer;
