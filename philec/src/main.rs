//
// main.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

//! `philec` is the command-line driver for the PHiLe Compiler.
//!
//! ## Basic usage:
//!
//! `philec -d mongodb -l go -o src/dal -n dbwrapper src/User.phi src/Post.phi`
//!
//! The absolute minimum of arguments to be provided are:
//!
//! * `-d`, `--database`: the name of the database engine to use.
//!   Currently, it is one of:
//!   * `sqlite3`
//!   * `mongodb`
//!   * `mariadb`
//!
//! * `-l`, `--language`: the programming language in which the
//!   generated Database Abstraction Layer will be emitted.
//!   Currently, it is one of:
//!   * `rust`
//!   * `c`
//!   * `cxx`
//!   * `objc`
//!   * `swift`
//!   * `go`
//!   * `js`
//!   * `python`
//!   * `ruby`
//!   * `java`
//!   * `csharp`
//!   * `haskell`
//!
//! * At least one PHiLe source file, typically with extension `.phi`.
//!
//! There are more command-line parameters, for greater flexibility:
//!
//! * A namespace can be specified using the `-n` or `--namespace`
//!   argument. Different backends treat it differently, in a way that
//!   is most idiomatic and most useful for the programming language
//!   in question. For example, in Go, it will be used for naming the
//!   package. For this reason, this argument is mandatory for the Go
//!   backend.
//! * The output directory of PHiLe is the current directory, `.`, by
//!   default. This can be changed via the `-o` or `--outdir` option.
//! * By default, PHiLe generates a DAL that retrieves values using
//!   POD (Plain Old Data) objects. These are immutable and do not
//!   automatically reflect changes in the underlying database. If
//!   you wish to use the Active Record strategy instead, which will
//!   create "smart" objects which automatically synchronize with the
//!   underlying storage, then you can pass the `-a` or `--access`
//!   parameter. The supported values for this argument are:
//!   * `pod` for Plain Old Data,
//!   * `acr` for Active Record.
//! * Since the PHiLe DSL probably has different stylistic conventions
//!   from those of your programming language of choice (unless that
//!   language happens to be Rust), names of program entities, such
//!   as types, functions, and class fields, need to be rewritten
//!   when generating the actual DAL. PHiLe has built-in defaults that
//!   are the most idiomatic/correct/useful for each supported
//!   language, but for maximal flexibility, all of them can be
//!   overridden. The following command line options are available for
//!   this purpose:
//!   * `-t`, `--typexform`: applied to user-defined type names (`class`es, `struct`s and `enum`s).
//!   * `-e`, `--fieldxform`: applied to `struct` and `class` field names.
//!   * `-v`, `--varxform`: applied to `enum` variant names.
//!   * `-f`, `--funcxform`: applied to function names.
//!   * `-s`, `--nsxform`: applied to namespace names.
//!
//!   Each of these parameters may take one of the following values:
//!
//!   * `default`: the default transform for the language will be applied.
//!   * `identity`: the name will be copied verbatim into the generated code.
//!   * `lowsnake`: the name will be transformed to `lower_snake_case`.
//!   * `upsnake`: the name will be transformed to `UPPER_SNAKE_CASE`.
//!   * `lowcamel`: the name will be transformed to `lowerCamelCase`.
//!   * `upcamel`: the name will be transformed to `UpperCamelCase`.
//!
//! ## Exit Status
//!
//! The command-line compiler exits with status `0` if the compilation
//! succeeds. If the compilation fails, it exits with a non-zero status,
//! after having removed all generated temporary files.
//!
//! # See Also
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
//! * [API documentation](https://docs.rs/phile/) for the `phile` library,
//!   useful when you want to embed PHiLe into your own Rust programs.

#![crate_name="philec"]
#![crate_type="bin"]
#![doc(html_root_url = "https://docs.rs/crate/philec/0.1.4")]
#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications, missing_docs)]
#![cfg_attr(feature = "cargo-clippy",
            allow(match_same_arms, clone_on_ref_ptr))]
#![cfg_attr(feature = "cargo-clippy",
            deny(wrong_pub_self_convention, used_underscore_binding,
                 stutter, similar_names, pub_enum_variant_names,
                 non_ascii_literal, unicode_not_nfc,
                 /* result_unwrap_used, option_unwrap_used, */ // TODO(H2CO3): fix these
                 option_map_unwrap_or_else, option_map_unwrap_or, filter_map,
                 shadow_unrelated, shadow_reuse, shadow_same,
                 int_plus_one, string_add_assign, if_not_else,
                 invalid_upcast_comparisons,
                 cast_sign_loss, /* cast_precision_loss, */ // TODO(H2CO3): fix these
                 cast_possible_wrap, cast_possible_truncation,
                 mutex_integer, mut_mut, items_after_statements,
                 print_stdout, mem_forget, maybe_infinite_iter))]

#[macro_use]
extern crate clap;
extern crate phile;

use std::collections::HashMap;
use std::str;
use std::fs::{ self, File };
use std::path::{ Path, PathBuf };
use std::time::Instant;
use std::rc::Rc;
use std::cell::RefCell;
use std::io::stderr;
use std::io::prelude::*;
use phile::util::{ RcCell, Diagnostic, DiagnosticKind, PACKAGE_INFO };
use phile::lexer::*;
use phile::parser::*;
use phile::sqirgen::*;
use phile::sqiropt::*;
use phile::dalgen::*;
use phile::error::*;


// Reporting elapsed time for each stage of the compiler pipeline
macro_rules! stopwatch {
    ($msg: expr, $code: expr) => ({
        eprint!("    {:.<40}", $msg);
        stderr().flush().expect("Could not flush stderr");
        let t0 = Instant::now();
        let val = $code;
        let t1 = Instant::now();
        let dt = t1 - t0;
        let secs = dt.as_secs() as f64 + f64::from(dt.subsec_nanos()) * 1e-9;
        let message = format!("{:6.1} ms", secs * 1e3);
        eprintln!("{}", Diagnostic::new(message, DiagnosticKind::Info));
        val
    })
}

// TODO(H2CO3): Rewrite this using `RcCell` once custom smart pointers
//              can point to trait objects, i.e. when `CoerceUnsized`
//              and `Unsize` are stabilized (see issue #27732).
struct FileWriterProvider {
    files: HashMap<PathBuf, Rc<RefCell<Write>>>,
    base_path: PathBuf,
    outfile_prefix: String,
}

impl FileWriterProvider {
    fn new(args: &CmdArgs) -> Self {
        FileWriterProvider {
            files: Default::default(),
            base_path: PathBuf::from(&args.output_directory),
            outfile_prefix: args.outfile_prefix.clone(),
        }
    }

    fn writer_with_name(&mut self, name: &str) -> Result<Rc<RefCell<Write>>> {
        let path = self.base_path.join(self.outfile_prefix.clone() + name);

        if let Some(rc) = self.files.get(&path) {
            return Ok(rc.clone())
        }

        let file = File::create(&path)?;
        let rc: Rc<RefCell<Write>> = Rc::new(RefCell::new(file));

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

//
// Parsing Command-Line Arguments
//

type ArgResult<T> = std::result::Result<T, String>;

#[derive(Debug)]
struct CmdArgs {
    codegen_params: CodegenParams,
    output_directory: String,
    outfile_prefix: String,
    migration_script: Option<String>,
    sources: Vec<String>,
}

impl CmdArgs {
    fn new() -> ArgResult<Self> {
        let args = clap_app!(philec =>
            (name:    PACKAGE_INFO.name)
            (version: PACKAGE_INFO.version)
            (author:  PACKAGE_INFO.authors)
            (about:   PACKAGE_INFO.description)
            (@arg database:    -d --database   +takes_value +required "Database engine")
            (@arg language:    -l --language   +takes_value +required "Wrapping language")
            (@arg access:      -a --access     +takes_value           "Database access mode")
            (@arg namespace:   -n --namespace  +takes_value           "Namespace for types and methods")
            (@arg type_xform:  -t --typexform  +takes_value           "Type name transform")
            (@arg field_xform: -e --fieldxform +takes_value           "Struct field name transform")
            (@arg var_xform:   -v --varxform   +takes_value           "Enum variant name transform")
            (@arg func_xform:  -f --funcxform  +takes_value           "Function name transform")
            (@arg ns_xform:    -s --nsxform    +takes_value           "Namespace name transform")
            (@arg outdir:      -o --outdir     +takes_value           "Output directory")
            (@arg outprefix:   -p --outprefix  +takes_value           "Filename prefix for output files")
            (@arg migrate:     -m --migrate    +takes_value           "Script to use for schema migration")
            (@arg sources:     +multiple                    +required "One or more PHiLe files")
        ).get_matches();

        let codegen_params = CodegenParams {
            database:               Self::database(args.value_of("database").unwrap())?,
            language:               Self::language(args.value_of("language").unwrap())?,
            database_access_mode:   Self::access_mode(args.value_of("access"))?,
            namespace:              args.value_of("namespace").map(str::to_owned),
            type_name_transform:    Self::name_transform(args.value_of("type_xform"))?,
            field_name_transform:   Self::name_transform(args.value_of("field_xform"))?,
            variant_name_transform: Self::name_transform(args.value_of("var_xform"))?,
            func_name_transform:    Self::name_transform(args.value_of("func_xform"))?,
            namespace_transform:    Self::name_transform(args.value_of("ns_xform"))?,
        };

        let args = CmdArgs {
            codegen_params:   codegen_params,
            output_directory: args.value_of("outdir").unwrap_or(".").to_owned(),
            outfile_prefix:   args.value_of("outprefix").unwrap_or("").to_owned(),
            migration_script: args.value_of("migrate").map(str::to_owned),
            sources:          args.values_of("sources").unwrap().map(str::to_owned).collect(),
        };

        Ok(args)
    }

    fn database(dbname: &str) -> ArgResult<DatabaseEngine> {
        Ok(match dbname {
            "sqlite3" => DatabaseEngine::SQLite3,
            "mongodb" => DatabaseEngine::MongoDB,
            "mariadb" => DatabaseEngine::MariaDB,
            _         => Self::arg_error("database engine", dbname)?,
        })
    }

    fn language(langname: &str) -> ArgResult<Language> {
        Ok(match langname {
            "rust"    => Language::Rust,
            "c"       => Language::C,
            "cxx"     => Language::CXX,
            "objc"    => Language::ObjectiveC,
            "swift"   => Language::Swift,
            "go"      => Language::Go,
            "js"      => Language::JavaScript,
            "python"  => Language::Python,
            "ruby"    => Language::Ruby,
            "java"    => Language::Java,
            "csharp"  => Language::CSharp,
            "haskell" => Language::Haskell,
            _         => Self::arg_error("language", langname)?,
        })
    }

    fn access_mode(mode: Option<&str>) -> ArgResult<Option<DatabaseAccessMode>> {
        Ok(match mode {
            None        => None,
            Some("pod") => Some(DatabaseAccessMode::Pod),
            Some("acr") => Some(DatabaseAccessMode::ActiveRecord),
            Some(value) => Self::arg_error("DB access mode", value)?,
        })
    }

    fn name_transform(transform: Option<&str>) -> ArgResult<Option<NameTransform>> {
        Ok(match transform {
            None             => None,
            Some("default")  => None,
            Some("identity") => Some(NameTransform::Identity),
            Some("lowsnake") => Some(NameTransform::LowerSnakeCase),
            Some("upsnake")  => Some(NameTransform::UpperSnakeCase),
            Some("lowcamel") => Some(NameTransform::LowerCamelCase),
            Some("upcamel")  => Some(NameTransform::UpperCamelCase),
            Some(value)      => Self::arg_error("name transform", value)?,
        })
    }

    fn arg_error<T>(name: &str, value: &str) -> ArgResult<T> {
        Err(format!("Invalid {}: '{}'", name, Diagnostic::new(value, DiagnosticKind::Error)))
    }
}

//
// I/O
//

fn read_file<P: AsRef<Path>>(path: P) -> Result<String> {
    let mut buf = String::new();
    let mut file = File::open(path)?;
    file.read_to_string(&mut buf)?;
    Ok(buf)
}

//
// Entry point
//

#[cfg_attr(feature = "cargo-clippy", allow(let_unit_value))]
fn philec_main(args: &CmdArgs, wp: &mut WriterProvider) -> Result<()> {
    let sources: Vec<_> = stopwatch!("Reading Sources", {
        args.sources.iter().map(read_file).collect::<Result<_>>()?
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

    let args = CmdArgs::new().unwrap_or_else(|error| {
        eprint!("    {}\n\n", error);
        std::process::exit(1);
    });

    let wp0 = RcCell::new(FileWriterProvider::new(&args));
    let wp1 = wp0.clone();
    let result = philec_main(&args, &mut move |name| wp0.borrow_mut()?.writer_with_name(name));

    // Handle errors by printing them, removing partially-written files, then bailing out
    result.unwrap_or_else(|error| {
        error.pretty_print(&mut stderr(), &args.sources).unwrap();
        wp1.borrow_mut().unwrap().remove_files();
        std::process::exit(1);
    });

    eprintln!();
    eprintln!("    {}", Diagnostic::new("Compilation Successful", DiagnosticKind::Success));
    eprintln!();
}
