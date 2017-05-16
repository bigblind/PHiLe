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
use std::fs::File;
use std::path::Path;
use std::error::Error;
use std::io::prelude::*;
use phile::lexer::*;
use phile::parser::*;


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
        database: database.to_owned(),
        wrapper:  wrapper.to_owned(),
        schemas:  schemas.map(str::to_owned).collect(),
    }
}

fn read_files(paths: &[&str]) -> Result<String, std::io::Error> {
    let mut buf = String::new();

    for path in paths {
        let mut file = File::open(&Path::new(path))?;
        file.read_to_string(&mut buf)?;
    }

    Ok(buf)
}

fn format_parse_error(error: &ParseError) -> String {
    let range = error.range.map_or("end of input".to_owned(), |r| format!("{:#?}", r));
    format!("Parse error near {}: {}", range, error.message)
}

fn main() {
    println!("The PHiLe Compiler");
    println!("Copyright (C) Arpad Goretity, 2017");
    println!();

    let args = get_args();

    let source = read_files(args.schemas).unwrap_or_else(
        |error| panic!("error reading file: {}", error.description())
    );

    let mut tokens = lex(&source).unwrap_or_else(
        |location| panic!("Lexer error at {:#?}", location)
    );

    tokens.retain(
        |token| match token.kind {
            TokenKind::Whitespace => false,
            TokenKind::Comment    => false,
            _                     => true,
        }
    );

    let program = parse(&tokens).unwrap_or_else(
        |error| panic!(format_parse_error(&error))
    );

    println!("{:#?}", program);
}
