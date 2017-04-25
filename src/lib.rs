//
// lib.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

#![crate_name="phile"]
#![crate_type="rlib"]
#![crate_type="dylib"]

#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications
        /*, missing_docs */)]

extern crate regex;
extern crate unicode_segmentation;

pub mod lexer;
pub mod ast;
pub mod parser;
pub mod semanal;
pub mod qir;
pub mod qirgen;
pub mod qiropt;
pub mod querygen;
pub mod wrapgen;
