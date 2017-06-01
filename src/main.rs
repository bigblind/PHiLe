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
use std::io;
use std::io::stdout;
use std::io::prelude::*;
use phile::lexer::*;
use phile::parser::*;
use phile::sqirgen::*;


#[derive(Debug)]
struct ProgramArgs {
    database: String,
    wrapper:  String,
    sources:  Vec<String>,
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
        (@arg sources:  +multiple +required "one or more PHiLe files")
    ).get_matches();

    let database = args.value_of("database").unwrap();
    let wrapper  = args.value_of("wrapper").unwrap();
    let sources  = args.values_of("sources").unwrap();

    ProgramArgs {
        database: database.to_owned(),
        wrapper:  wrapper.to_owned(),
        sources:  sources.map(str::to_owned).collect(),
    }
}

fn read_file(path: &str) -> io::Result<String> {
    let mut buf = String::new();
    let mut file = File::open(&Path::new(path))?;
    file.read_to_string(&mut buf)?;
    Ok(buf)
}

fn read_files<P: AsRef<str>>(paths: &[P]) -> io::Result<Vec<String>> {
    paths.iter().map(|p| read_file(p.as_ref())).collect()
}

fn format_lexer_error<P: AsRef<str>>(location: &Location, files: &[P]) -> String {
    let file = files[location.src_idx].as_ref();
    format!("Lexical error in '{}' near {}", file, location)
}

fn format_parse_error<P: AsRef<str>>(error: &ParseError, files: &[P]) -> String {
    let file = error.range.map_or("source file", |r| files[r.begin.src_idx].as_ref());
    let range = error.range.map_or("end of input".to_owned(), |r| format!("{}", r));
    format!("Parse error in '{}' near {}: {}", file, range, error.message)
}

fn format_sema_error<P: AsRef<str>>(error: &SemaError, files: &[P]) -> String {
    let file = error.range.map_or("source file", |r| files[r.begin.src_idx].as_ref());
    let range = error.range.map_or("end of input".to_owned(), |r| format!("{}", r));
    format!("Semantic error in '{}' near {}: {}", file, range, error.message)
}

fn main() {
    println!("The PHiLe Compiler");
    println!("Copyright (C) Arpad Goretity, 2017");
    println!();

    let args = get_args();

    let sources = stopwatch!("Reading Sources", {
        read_files(&args.sources).unwrap_or_else(
            |error| panic!("Error reading file: {}", error.description())
        )
    });

    let tokens = stopwatch!("Lexing", {
        let mut tmp_tokens = lex(&sources).unwrap_or_else(
            |location| panic!(format_lexer_error(&location, &args.sources))
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
            |error| panic!(format_parse_error(&error, &args.sources))
        )
    });

    let sqir = stopwatch!("Typechecking and generating SQIR", {
        generate_sqir(&program).unwrap_or_else(
            |error| panic!(format_sema_error(&error, &args.sources))
        )
    });

    println!();
    println!("{:#?}", sqir);
    println!();
    println!("Compilation Successful");
}
