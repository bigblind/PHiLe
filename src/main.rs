//
// main.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

#[macro_use]
extern crate clap;
extern crate phile;

use std::str;
use std::slice;
use std::fs::File;
use std::path::Path;
use std::error::Error;
use std::io::prelude::*;
use phile::lexer::{Lexer, TokenKind};


#[derive(Debug)]
struct ProgramArgs {
    database: String,
    wrapper:  String,
    schemas:  Vec<String>,
}


fn get_args() -> ProgramArgs {
    let args = clap_app!(app =>
        (name:    env!["CARGO_PKG_NAME"])
        (version: env!["CARGO_PKG_VERSION"])
        (author:  env!["CARGO_PKG_AUTHORS"])
        (about:   env!["CARGO_PKG_DESCRIPTION"])
        (@arg database: -d --database +takes_value +required "database flavor, e.g. SQLite, MySQL, ...")
        (@arg wrapper:  -w --wrapper  +takes_value +required "wrapping language, e.g. Rust, C, ...")
        (@arg schemas:  +multiple +required "one or more PHiLe files")
    ).get_matches();

    let database = args.value_of("database").unwrap();
    let wrapper  = args.value_of("wrapper").unwrap();
    let schemas  = args.values_of("schemas").unwrap();

    ProgramArgs {
        database: database.to_string(),
        wrapper:  wrapper.to_string(),
        schemas:  schemas.map(str::to_string).collect(),
    }
}

fn read_file(path: &str) -> Result<String, std::io::Error> {
    let mut file = try!(File::open(&Path::new(path)));
    let mut buf = String::new();
    try!(file.read_to_string(&mut buf));
    Ok(buf)
}

fn is_subslice(haystack: &str, needle: &str) -> bool {
    let haystack_begin = haystack.as_ptr();
    let haystack_end   = unsafe { haystack.as_ptr().offset(haystack.len() as isize) };
    let needle_begin   = needle.as_ptr();
    let needle_end     = unsafe { needle.as_ptr().offset(needle.len() as isize) };

    haystack_begin <= needle_begin && needle_end <= haystack_end
}

fn slice_between<'a>(haystack: &'a str, first: &'a str, last: &'a str) -> Option<&'a str> {
    if !is_subslice(haystack, first) || !is_subslice(haystack, last) {
        return None;
    }

    let first_begin = first.as_ptr();
    let first_end   = unsafe { first.as_ptr().offset(first.len() as isize) };
    let last_begin  = last.as_ptr();
    let last_end    = unsafe { last.as_ptr().offset(last.len() as isize) };

    // first and last can overlap, the only criterion is
    // that the beginning of first cannot be higher than
    // the beginning of the last, and the end of the last
    // cannot be lower than the end of the first.
    if last_begin < first_begin || last_end < first_end {
        return None;
    }

    unsafe {
        let length = last_end as usize - first_begin as usize;
        let bytes = slice::from_raw_parts(first_begin, length);
        str::from_utf8(bytes).ok()
    }
}

fn main() {
    println!("The PHiLe Compiler");
    println!("Copyright (C) Arpad Goretity, 2017");
    println!();

    let args = get_args();

    if args.schemas.len() != 1 {
        panic!("currently, only compiling one single file is supported");
    }

    let source = read_file(&args.schemas[0]).unwrap_or_else(
        |err| panic!("could not read file '{}': {}", args.schemas[0], err.description())
    );

    let mut tokens = Lexer::new(&source).lex().unwrap_or_else(
        |location| panic!("Lexer error at {:?}", location)
    );

    tokens.retain(
        |token| match token.kind {
            TokenKind::Whitespace => false,
            TokenKind::Comment    => false,
            _                     => true,
        }
    );

    for token in tokens {
        println!("{:?}", token);
    }
}
