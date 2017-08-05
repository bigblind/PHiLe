//
// dalgen/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

mod rust;
mod c;
mod cxx;
mod objc;
mod swift;
mod go;
mod js;
mod python;
mod java;

use error::Result;
use codegen::*;
use sqir::*;


macro_rules! call_dalgen {
    ($module: ident, $sqir: expr, $params: expr, $wp: expr) => {
        match $params.database_access_mode {
            DatabaseAccessMode::Pod          => $module::generate_pod($sqir, $params, $wp),
            DatabaseAccessMode::ActiveRecord => $module::generate_active_record($sqir, $params, $wp),
        }
    }
}

pub fn generate_dal(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    match params.language {
        Language::Rust       => call_dalgen!(rust,   sqir, params, wp),
        Language::C          => call_dalgen!(c,      sqir, params, wp),
        Language::CXX        => call_dalgen!(cxx,    sqir, params, wp),
        Language::ObjectiveC => call_dalgen!(objc,   sqir, params, wp),
        Language::Swift      => call_dalgen!(swift,  sqir, params, wp),
        Language::Go         => call_dalgen!(go,     sqir, params, wp),
        Language::JavaScript => call_dalgen!(js,     sqir, params, wp),
        Language::Python     => call_dalgen!(python, sqir, params, wp),
        Language::Java       => call_dalgen!(java,   sqir, params, wp),
    }
}
