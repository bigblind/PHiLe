//
// codegen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 01/06/2017
//

use std::io;
use sqir::SQIR;
use declgen;
use schemagen;
use querygen;


#[derive(Debug, Clone, Copy)]
pub enum DatabaseEngine {
    SQLite3,
    MongoDB,
    MariaDB,
}

#[derive(Debug, Clone, Copy)]
pub enum Language {
    Rust,
    C,
    CXX,
    ObjectiveC,
    Go,
    JavaScript,
    Python,
    Java,
}

#[derive(Debug, Clone, Copy)]
pub enum DatabaseAccessMode {
    POD,
    ActiveRecord,
}

#[derive(Debug, Clone, Copy)]
pub enum NameTransform {
    Identity,
    DefaultForLanguage,
    LowerCamelCase,
    UpperCamelCase,
    SnakeCase, // isn't SnakeCase spelled with camel case ironic?
}

#[derive(Debug)]
pub struct CodegenParams {
    pub database:             DatabaseEngine,
    pub language:             Language,
    pub out_filename_prefix:  Option<String>,
    pub database_access_mode: DatabaseAccessMode,
    pub namespace:            Option<String>,
    pub type_name_transform:  NameTransform,
    pub field_name_transform: NameTransform,
    pub func_name_transform:  NameTransform,
    pub namespace_transform:  NameTransform,
}

#[allow(missing_debug_implementations)]
pub enum CodegenOutput<'a> {
    Directory(&'a str),
    Writer(&'a io::Write),
}


macro_rules! call_declgen {
    ($module: ident, $sqir: expr, $params: expr, $out: expr) => {
        match $params.database_access_mode {
            DatabaseAccessMode::POD          => declgen::$module::generate_pod($sqir, $params, $out),
            DatabaseAccessMode::ActiveRecord => declgen::$module::generate_active_record($sqir, $params, $out),
        }
    }
}


pub fn generate_declarations(sqir: &SQIR, params: &CodegenParams, out: &CodegenOutput) -> io::Result<()> {
    match params.language {
        Language::Rust       => call_declgen!(rust,   sqir, params, out),
        Language::C          => call_declgen!(c,      sqir, params, out),
        Language::CXX        => call_declgen!(cxx,    sqir, params, out),
        Language::ObjectiveC => call_declgen!(objc,   sqir, params, out),
        Language::Go         => call_declgen!(go,     sqir, params, out),
        Language::JavaScript => call_declgen!(js,     sqir, params, out),
        Language::Python     => call_declgen!(python, sqir, params, out),
        Language::Java       => call_declgen!(java,   sqir, params, out),
    }
}

pub fn generate_schema(sqir: &SQIR, params: &CodegenParams, out: &CodegenOutput) -> io::Result<()> {
    match params.database {
        DatabaseEngine::SQLite3 => schemagen::sqlite3::generate(sqir, params, out),
        DatabaseEngine::MongoDB => schemagen::mongo::generate(sqir, params, out),
        DatabaseEngine::MariaDB => schemagen::maria::generate(sqir, params, out),
    }
}

pub fn generate_queries(sqir: &SQIR, params: &CodegenParams, out: &CodegenOutput) -> io::Result<()> {
    match params.database {
        DatabaseEngine::SQLite3 => querygen::sqlite3::generate(sqir, params, out),
        DatabaseEngine::MongoDB => querygen::mongo::generate(sqir, params, out),
        DatabaseEngine::MariaDB => querygen::maria::generate(sqir, params, out),
    }
}

pub fn access_mode_error<T>(params: &CodegenParams) -> io::Result<T> {
    let message = format!(
        "Backend '{:#?}/{:#?}' does not support DB access mode '{:#?}'",
        params.database,
        params.language,
        params.database_access_mode
    );
    Err(io::Error::new(io::ErrorKind::InvalidInput, message))
}
