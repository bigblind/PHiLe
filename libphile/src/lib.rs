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
//! * `parser` performs higher-level syntactic analysis and outputs an…
//! * `ast`, an Abstract Syntax Tree.
//! * `sqirgen` takes the AST and typechecks it, then emits…
//! * `sqir`, the Schema and Query Intermediate Representation, PHiLe IR.
//! * `sqiropt` will take the raw SQIR and optimize it, so that it can be fed into…
//! * `dalgen`, which is the back-end that generates the actual DAL code.
//! * `util` contains miscellaneous helper types and functions.
//! * `error` contains type definitions for uniformly describing syntactic,
//!   semantic, and internal compiler errors.
//!
//! Depending on how you are willing to use PHiLe, you may be looking for…
//!
//! * [The Tutorial](https://h2co3.github.io/phile/tutorial). This gets you
//!   started quickly and painlessly with writing schemas and queries in
//!   PHiLe's domain-specific language.
//! * [The Examples](https://github.com/H2CO3/PHiLe/tree/master/doc/examples).
//!   Check out these code snippets if you learn easier by example.
//! * [The Reference](https://h2co3.github.io/phile/reference).
//!   Search through this document if you are already familiar with the basics
//!   and you are now looking for the details of a specific feature.
//! * [Manpage-style docs for `philec`](https://docs.rs/philec/),
//!   if you want to deep dive into the invocation of the PHiLe CLI compiler.


#![crate_name="phile"]
#![crate_type="rlib"]
#![crate_type="dylib"]
#![doc(html_root_url = "https://docs.rs/phile/0.1.3")]
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
