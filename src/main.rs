#[macro_use]
extern crate clap;
extern crate phile;

// use phile::{lexer, ast, parser, semanal, qir, qirgen, qiropt, querygen, wrapgen};
use std::fs::File;
use std::path::Path;
use std::error::Error;
use std::io::prelude::*;
use phile::lexer::Lexer;

#[derive(Debug)]
struct ProgramArgs {
    database: String,
    wrapper:  String,
    schemas:  Vec<String>,
}

fn read_file(path: &str) -> Result<String, std::io::Error> {
    let mut file = try!(File::open(&Path::new(path)));
    let mut buf = String::new();
    try!(file.read_to_string(&mut buf));
    Ok(buf)
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

    let tokens = Lexer::new(&source).lex().unwrap_or_else(
        |location| panic!("Lexer error at {:?}", location)
    );

    for token in tokens {
        println!("{:?}", token);
    }
}
