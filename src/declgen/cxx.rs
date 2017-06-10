//
// declgen/cxx.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

use std::io;
use codegen::*;
use sqir::*;


pub fn generate_pod(_sqir: &SQIR, _params: &CodegenParams, _wp: &mut WriterProvider) -> io::Result<()> {
    unimplemented!()
}

pub fn generate_active_record(_sqir: &SQIR, _params: &CodegenParams, _wp: &mut WriterProvider) -> io::Result<()> {
    unimplemented!()
}
