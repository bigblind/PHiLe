//
// dalgen/mongodb/mod.rs
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


pub fn generate(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    match params.language {
        Language::Rust       => rust  ::generate(sqir, params, wp),
        Language::C          => c     ::generate(sqir, params, wp),
        Language::CXX        => cxx   ::generate(sqir, params, wp),
        Language::ObjectiveC => objc  ::generate(sqir, params, wp),
        Language::Swift      => swift ::generate(sqir, params, wp),
        Language::Go         => go    ::generate(sqir, params, wp),
        Language::JavaScript => js    ::generate(sqir, params, wp),
        Language::Python     => python::generate(sqir, params, wp),
        Language::Java       => java  ::generate(sqir, params, wp),
    }
}
