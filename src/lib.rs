//
// lib.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

//! This library provides the programmatic interface for the PHiLe Compiler
//! and Domain-Specific Language. The crate is composed of several modules,
//! each of which roughly corresponds to a single step in the compilation
//! pipeline:
//!
//! * `lexer` performs lexical analysis and tokenization.
//! * `parser` performs higher-level syntactic analysis and outputs an...
//! * `ast`, an Abstract Syntax Tree.
//! * `sqirgen` takes the AST and typechecks it, then emits...
//! * `sqir`, the Schema and Query Intermediate Representation, PHiLe IR.
//! * `sqiropt` will take the raw SQIR and optimize it, so that it can be fed into...
//! * `dalgen`, which is the back-end that generates the actual DAL code.
//! * `util` contains miscellaneous helper types and functions.
//! * `error` contains type definitions for uniformly describing syntactic,
//!   semantic, and internal compiler errors.

#![crate_name="phile"]
#![crate_type="rlib"]
#![crate_type="dylib"]

#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications, missing_docs)]

extern crate regex;
extern crate heck;
extern crate unicode_segmentation;

#[macro_use]
pub mod util;
#[macro_use]
pub mod error;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod sqir;
pub mod sqirgen;
pub mod sqiropt;
pub mod dalgen;
