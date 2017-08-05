//
// dalgen/cxx/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 18/07/2017
//

use error::Result;
use codegen::*;
use sqir::*;


pub fn generate_pod(_sqir: &Sqir, _params: &CodegenParams, _wp: &mut WriterProvider) -> Result<()> {
    unimplemented!()
}

pub fn generate_active_record(_sqir: &Sqir, _params: &CodegenParams, _wp: &mut WriterProvider) -> Result<()> {
    unimplemented!()
}
