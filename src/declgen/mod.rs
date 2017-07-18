//
// declgen/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

pub mod rust;
pub mod c;
pub mod cxx;
pub mod objc;
pub mod swift;
pub mod go;
pub mod js;
pub mod python;
pub mod java;

use std::io;
use codegen::*;
use sqir::*;


macro_rules! call_declgen {
    ($module: ident, $sqir: expr, $params: expr, $wp: expr) => {
        match $params.database_access_mode {
            DatabaseAccessMode::POD          => $module::generate_pod($sqir, $params, $wp),
            DatabaseAccessMode::ActiveRecord => $module::generate_active_record($sqir, $params, $wp),
        }
    }
}

pub fn generate_declarations(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    match params.language {
        Language::Rust       => call_declgen!(rust,   sqir, params, wp),
        Language::C          => call_declgen!(c,      sqir, params, wp),
        Language::CXX        => call_declgen!(cxx,    sqir, params, wp),
        Language::ObjectiveC => call_declgen!(objc,   sqir, params, wp),
        Language::Swift      => call_declgen!(swift,  sqir, params, wp),
        Language::Go         => call_declgen!(go,     sqir, params, wp),
        Language::JavaScript => call_declgen!(js,     sqir, params, wp),
        Language::Python     => call_declgen!(python, sqir, params, wp),
        Language::Java       => call_declgen!(java,   sqir, params, wp),
    }
}
