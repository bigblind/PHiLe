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
use std::time::Instant;
use std::io::stdout;
use std::io::prelude::*;
use phile::lexer::*;
use phile::parser::*;
use phile::sqirgen::*;


#[derive(Debug)]
struct ProgramArgs {
    database: String,
    wrapper:  String,
    schemas:  Vec<String>,
}


macro_rules! stopwatch {
    ($msg: expr, $code: expr) => ({
        print!("{}... ", $msg);
        stdout().flush().expect("Could not flush stdout");
        let t0 = Instant::now();
        let val = $code;
        let t1 = Instant::now();
        let dt = t1 - t0;
        let secs = dt.as_secs() as f64 + dt.subsec_nanos() as f64 * 1e-9;
        println!("{:.0} ms", secs * 1e3);
        val
    })
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

fn read_files<T: AsRef<str>>(paths: &[T]) -> Result<String, std::io::Error> {
    let mut buf = String::new();

    for path in paths {
        let mut file = File::open(&Path::new(path.as_ref()))?;
        file.read_to_string(&mut buf)?;
    }

    Ok(buf)
}

fn format_parse_error(error: &ParseError) -> String {
    let range = error.range.map_or("end of input".to_owned(), |r| format!("{:#?}", r));
    format!("Parse error near {}: {}", range, error.message)
}

fn format_sema_error(error: &SemaError) -> String {
    let range = error.range.map_or("end of input".to_owned(), |r| format!("{:#?}", r));
    format!("Semantic error near {}: {}", range, error.message)
}

fn main() {
    println!("The PHiLe Compiler");
    println!("Copyright (C) Arpad Goretity, 2017");
    println!();

    let args = get_args();

    let source = stopwatch!("Reading Sources", {
        read_files(&args.schemas).unwrap_or_else(
            |error| panic!("error reading file: {}", error.description())
        )
    });

    let tokens = stopwatch!("Lexing", {
        let mut tmp_tokens = lex(&source).unwrap_or_else(
            |location| panic!("Lexer error at {:#?}", location)
        );

        tmp_tokens.retain(
            |token| match token.kind {
                TokenKind::Whitespace => false,
                TokenKind::Comment    => false,
                _                     => true,
            }
        );

        tmp_tokens
    });

    let program = stopwatch!("Parsing", {
        parse(&tokens).unwrap_or_else(
            |error| panic!(format_parse_error(&error))
        )
    });

    let sqir = stopwatch!("Typechecking and generating SQIR", {
        generate_sqir(&program).unwrap_or_else(
            |error| panic!(format_sema_error(&error))
        )
    });

    println!("{:#?}", sqir);

    println!("Compilation Successful");
}
