//
// declgen/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

use std::io;
use codegen::*;
use sqir::*;


pub fn generate(sqir: &SQIR, params: &CodegenParams, out: &CodegenOutput) -> io::Result<()> {
    match params.database_access_mode {
        DatabaseAccessMode::POD          => generate_pod(sqir, params, out),
        DatabaseAccessMode::ActiveRecord => generate_active_record(sqir, params, out),
    }
}

fn generate_pod(sqir: &SQIR, params: &CodegenParams, out: &CodegenOutput) -> io::Result<()> {
    unimplemented!()
}

fn generate_active_record(sqir: &SQIR, params: &CodegenParams, out: &CodegenOutput) -> io::Result<()> {
    access_mode_error(params)
}
