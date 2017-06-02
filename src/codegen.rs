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


pub fn generate_declarations(sqir: &SQIR, params: &CodegenParams, out: &CodegenOutput) -> io::Result<()> {
    match params.language {
        Language::Rust       => declgen::rust::generate(sqir, params, out),
        Language::C          => declgen::c::generate(sqir, params, out),
        Language::CXX        => declgen::cxx::generate(sqir, params, out),
        Language::ObjectiveC => declgen::objc::generate(sqir, params, out),
        Language::Go         => declgen::go::generate(sqir, params, out),
        Language::JavaScript => declgen::js::generate(sqir, params, out),
        Language::Python     => declgen::python::generate(sqir, params, out),
        Language::Java       => declgen::java::generate(sqir, params, out),
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
