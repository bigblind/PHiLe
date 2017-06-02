//
// philec.rs
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
use phile::codegen::*;


#[derive(Debug)]
struct ProgramArgs {
    codegen_params:   CodegenParams,
    output_directory: String,
    migration_script: Option<String>,
    sources:          Vec<String>,
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
    let args = clap_app!(philec =>
        (name:    env!["CARGO_PKG_NAME"])
        (version: env!["CARGO_PKG_VERSION"])
        (author:  env!["CARGO_PKG_AUTHORS"])
        (about:   env!["CARGO_PKG_DESCRIPTION"])
        (@arg database:    -d --database   +takes_value +required "database engine")
        (@arg language:    -l --language   +takes_value +required "wrapping language")
        (@arg outprefix:   -p --outprefix  +takes_value           "filename prefix for output files")
        (@arg access:      -a --access     +takes_value           "database access mode")
        (@arg namespace:   -n --namespace  +takes_value           "namespace for types and methods")
        (@arg type_xform:  -t --typexform  +takes_value           "type name transform")
        (@arg field_xform: -e --fieldxform +takes_value           "struct field/enum variant name transform")
        (@arg func_xform:  -f --funcxform  +takes_value           "function name transform")
        (@arg ns_xform:    -s --nsxform    +takes_value           "namespace name transform")
        (@arg outdir:      -o --outdir     +takes_value           "output directory")
        (@arg migrate:     -m --migrate    +takes_value           "script to use for schema migration")
        (@arg sources:     +multiple                    +required "one or more PHiLe files")
    ).get_matches();

    let codegen_params = CodegenParams {
        database:             validate_database(args.value_of("database").unwrap()),
        language:             validate_language(args.value_of("language").unwrap()),
        out_filename_prefix:  args.value_of("outprefix").map(str::to_owned),
        database_access_mode: validate_access(args.value_of("access")),
        namespace:            args.value_of("namespace").map(str::to_owned),
        type_name_transform:  validate_name_transform(args.value_of("type_xform")),
        field_name_transform: validate_name_transform(args.value_of("field_xform")),
        func_name_transform:  validate_name_transform(args.value_of("func_xform")),
        namespace_transform:  validate_name_transform(args.value_of("ns_xform")),
    };

    ProgramArgs {
        codegen_params:   codegen_params,
        output_directory: args.value_of("outdir").unwrap_or(".").to_owned(),
        migration_script: args.value_of("migrate").map(str::to_owned),
        sources:          args.values_of("sources").unwrap().map(str::to_owned).collect(),
    }
}

fn validate_database(dbname: &str) -> DatabaseEngine {
    match dbname {
        "sqlite3" => DatabaseEngine::SQLite3,
        "mongo"   => DatabaseEngine::MongoDB,
        "maria"   => DatabaseEngine::MariaDB,
        _         => panic!("Unsupported database engine: '{}'", dbname),
    }
}

fn validate_language(langname: &str) -> Language {
    match langname {
        "rust"   => Language::Rust,
        "c"      => Language::C,
        "cxx"    => Language::CXX,
        "objc"   => Language::ObjectiveC,
        "go"     => Language::Go,
        "js"     => Language::JavaScript,
        "python" => Language::Python,
        "java"   => Language::Java,
        _        => panic!("Unsupported language: '{}'", langname),
    }
}

fn validate_access(modename: Option<&str>) -> DatabaseAccessMode {
    match modename {
        None              => DatabaseAccessMode::POD,
        Some("pod")       => DatabaseAccessMode::POD,
        Some("activerec") => DatabaseAccessMode::ActiveRecord,
        Some(name)        => panic!("Invalid DB access mode: '{}'", name),
    }
}

fn validate_name_transform(xformname: Option<&str>) -> NameTransform {
    match xformname {
        None             => NameTransform::DefaultForLanguage,
        Some("identity") => NameTransform::Identity,
        Some("default")  => NameTransform::DefaultForLanguage,
        Some("lowcamel") => NameTransform::LowerCamelCase,
        Some("upcamel")  => NameTransform::UpperCamelCase,
        Some("snake")    => NameTransform::SnakeCase,
        Some(name)       => panic!("Invalid name transform: '{}'", name),
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

    let output = CodegenOutput::Directory(&args.output_directory);

    stopwatch!("Generating Declarations", {
        generate_declarations(&sqir, &args.codegen_params, &output).unwrap_or_else(
            |error| panic!("Could not generate declarations: {}", error.description())
        )
    });

    stopwatch!("Generating Schema", {
        generate_schema(&sqir, &args.codegen_params, &output).unwrap_or_else(
            |error| panic!("Could not generate schema: {}", error.description())
        )
    });

    stopwatch!("Generating Queries", {
        generate_queries(&sqir, &args.codegen_params, &output).unwrap_or_else(
            |error| panic!("Could not generate queries: {}", error.description())
        )
    });

    println!();
    println!("Compilation Successful");
}
