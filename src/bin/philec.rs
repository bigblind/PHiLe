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

use std::collections::HashMap;
use std::str;
use std::fs::{ self, File };
use std::path::PathBuf;
use std::time::Instant;
use std::rc::Rc;
use std::cell::RefCell;
use std::io::{ self, stderr };
use std::io::prelude::*;
use phile::util::{ RcCell, COLOR, PACKAGE_INFO };
use phile::lexer::*;
use phile::parser::*;
use phile::sqirgen::*;
use phile::sqiropt::*;
use phile::codegen::*;
use phile::dalgen::*;
use phile::error::*;


#[derive(Debug)]
struct ProgramArgs {
    codegen_params:   CodegenParams,
    output_directory: String,
    outfile_prefix:   String,
    migration_script: Option<String>,
    sources:          Vec<String>,
}

// TODO(H2CO3): Rewrite this using RcCell once custom smart pointers
//              can point to trait objects, i.e. when CoerceUnsized
//              and Unsize are stabilized (see issue #27732)
struct FileWriterProvider {
    files: HashMap<PathBuf, Rc<RefCell<io::Write>>>,
    base_path: PathBuf,
    outfile_prefix: String,
}

impl FileWriterProvider {
    fn new(args: &ProgramArgs) -> Self {
        FileWriterProvider {
            files: Default::default(),
            base_path: PathBuf::from(&args.output_directory),
            outfile_prefix: args.outfile_prefix.clone(),
        }
    }

    fn writer_with_name(&mut self, name: &str) -> Result<Rc<RefCell<io::Write>>> {
        let path = self.base_path.join(self.outfile_prefix.clone() + name);

        if let Some(rc) = self.files.get(&path) {
            return Ok(rc.clone())
        }

        let file = File::create(&path)?;
        let rc = Rc::new(RefCell::new(file)) as Rc<RefCell<io::Write>>;

        self.files.insert(path, rc.clone());

        Ok(rc)
    }

    fn remove_files(&self) {
        for path in self.files.keys() {
            fs::remove_file(path).unwrap_or_else(
                |e| eprintln!("    Could not remove {}: {}", path.to_string_lossy(), e)
            )
        }
    }
}

macro_rules! stopwatch {
    ($msg: expr, $code: expr) => ({
        eprint!("    {:.<40}", $msg);
        stderr().flush().expect("Could not flush stderr");
        let t0 = Instant::now();
        let val = $code;
        let t1 = Instant::now();
        let dt = t1 - t0;
        let secs = dt.as_secs() as f64 + dt.subsec_nanos() as f64 * 1e-9;
        eprintln!("{}{:6.1} ms{}", COLOR.info, secs * 1e3, COLOR.reset);
        val
    })
}

//
// Parsing Command-Line Arguments
//

fn get_args() -> ProgramArgs {
    let args = clap_app!(philec =>
        (name:    PACKAGE_INFO.name)
        (version: PACKAGE_INFO.version)
        (author:  PACKAGE_INFO.authors)
        (about:   PACKAGE_INFO.description)
        (@arg database:    -d --database   +takes_value +required "database engine")
        (@arg language:    -l --language   +takes_value +required "wrapping language")
        (@arg access:      -a --access     +takes_value           "database access mode")
        (@arg namespace:   -n --namespace  +takes_value           "namespace for types and methods")
        (@arg type_xform:  -t --typexform  +takes_value           "type name transform")
        (@arg field_xform: -e --fieldxform +takes_value           "struct field name transform")
        (@arg var_xform:   -v --varxform   +takes_value           "enum variant name transform")
        (@arg func_xform:  -f --funcxform  +takes_value           "function name transform")
        (@arg ns_xform:    -s --nsxform    +takes_value           "namespace name transform")
        (@arg outdir:      -o --outdir     +takes_value           "output directory")
        (@arg outprefix:   -p --outprefix  +takes_value           "filename prefix for output files")
        (@arg migrate:     -m --migrate    +takes_value           "script to use for schema migration")
        (@arg sources:     +multiple                    +required "one or more PHiLe files")
    ).get_matches();

    let codegen_params = CodegenParams {
        database:               validate_database(args.value_of("database").unwrap()),
        language:               validate_language(args.value_of("language").unwrap()),
        database_access_mode:   validate_access(args.value_of("access")),
        namespace:              args.value_of("namespace").map(str::to_owned),
        type_name_transform:    validate_name_transform(args.value_of("type_xform")),
        field_name_transform:   validate_name_transform(args.value_of("field_xform")),
        variant_name_transform: validate_name_transform(args.value_of("var_xform")),
        func_name_transform:    validate_name_transform(args.value_of("func_xform")),
        namespace_transform:    validate_name_transform(args.value_of("ns_xform")),
    };

    ProgramArgs {
        codegen_params:   codegen_params,
        output_directory: args.value_of("outdir").unwrap_or(".").to_owned(),
        outfile_prefix:   args.value_of("outprefix").unwrap_or("").to_owned(),
        migration_script: args.value_of("migrate").map(str::to_owned),
        sources:          args.values_of("sources").unwrap().map(str::to_owned).collect(),
    }
}

fn validate_database(dbname: &str) -> DatabaseEngine {
    match dbname {
        "sqlite3" => DatabaseEngine::SQLite3,
        "mongodb" => DatabaseEngine::MongoDB,
        "mariadb" => DatabaseEngine::MariaDB,
        _         => handle_argument_error("database engine", dbname),
    }
}

fn validate_language(langname: &str) -> Language {
    match langname {
        "rust"   => Language::Rust,
        "c"      => Language::C,
        "cxx"    => Language::CXX,
        "objc"   => Language::ObjectiveC,
        "swift"  => Language::Swift,
        "go"     => Language::Go,
        "js"     => Language::JavaScript,
        "python" => Language::Python,
        "java"   => Language::Java,
        _        => handle_argument_error("language", langname),
    }
}

fn validate_access(mode: Option<&str>) -> DatabaseAccessMode {
    mode.map_or(
        DatabaseAccessMode::Pod,
        |name| match name {
            "pod" => DatabaseAccessMode::Pod,
            "acr" => DatabaseAccessMode::ActiveRecord,
            _     => handle_argument_error("DB access mode", name),
        }
    )
}

fn validate_name_transform(transform: Option<&str>) -> Option<NameTransform> {
    transform.and_then(|name| match name {
        "default"  => None,
        "identity" => Some(NameTransform::Identity),
        "lowsnake" => Some(NameTransform::LowerSnakeCase),
        "upsnake"  => Some(NameTransform::UpperSnakeCase),
        "lowcamel" => Some(NameTransform::LowerCamelCase),
        "upcamel"  => Some(NameTransform::UpperCamelCase),
        _          => handle_argument_error("name transform", name),
    })
}

//
// I/O
//

fn read_file(path: &str) -> io::Result<String> {
    let mut buf = String::new();
    let mut file = File::open(path)?;
    file.read_to_string(&mut buf)?;
    Ok(buf)
}

fn read_files<P: AsRef<str>>(paths: &[P]) -> io::Result<Vec<String>> {
    paths.iter().map(|p| read_file(p.as_ref())).collect()
}

//
// Error Reporting
//

fn handle_argument_error(arg_name: &str, value: &str) -> ! {
    eprint!(
        "    Invalid {arg_name}: {clr_err}'{value}'{clr_rst}\n\n",
        arg_name = arg_name,
        value = value,
        clr_err = COLOR.error,
        clr_rst = COLOR.reset,
    );
    ::std::process::exit(1)
}

//
// Entry point
//

fn philec_main(args: &ProgramArgs, wp: &mut WriterProvider) -> Result<()> {
    let sources = stopwatch!("Reading Sources", {
        read_files(&args.sources)?
    });

    let tokens = stopwatch!("Lexing", {
        let mut tokens = lex(&sources)?;

        tokens.retain(|token| match token.kind {
            TokenKind::Whitespace => false,
            TokenKind::Comment    => false,
            _                     => true,
        });

        tokens
    });

    let program = stopwatch!("Parsing", {
        parse(&tokens)?
    });

    let sqir = stopwatch!("Typechecking and generating SQIR", {
        generate_sqir(&program)?
    });

    let sqir = stopwatch!("Optimizing SQIR", {
        optimize_sqir(sqir)
    });

    let result = stopwatch!("Generating Database Abstraction Layer", {
        generate_dal(&sqir, &args.codegen_params, wp)?
    });

    Ok(result)
}

fn main() {
    eprintln!();
    eprintln!("    The PHiLe Compiler, version {}", PACKAGE_INFO.version);
    eprintln!("    Copyright (C) 2017, {}", PACKAGE_INFO.authors);
    eprintln!();

    let args = get_args();
    let wp0 = RcCell::new(FileWriterProvider::new(&args));
    let wp1 = wp0.clone();
    let result = philec_main(&args, &mut move |name| wp0.borrow_mut()?.writer_with_name(name));

    // Handle errors by printing them, removing partially-written files, then bailing out
    result.unwrap_or_else(|error| {
        error.pretty_print(&mut io::stderr(), &args.sources).unwrap();
        wp1.borrow_mut().unwrap().remove_files();
        std::process::exit(1);
    });

    eprintln!();
    eprintln!("    {}Compilation Successful{}", COLOR.success, COLOR.reset);
    eprintln!();
}
