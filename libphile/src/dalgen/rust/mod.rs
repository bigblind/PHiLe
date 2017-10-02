//
// dalgen/rust/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 18/07/2017
//

use error::Result;
use dalgen::*;
use sqir::*;


pub fn generate_dal(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    match params.database_access_mode.unwrap_or_default() {
        DatabaseAccessMode::Pod          => generate_pod(sqir, params, wp),
        DatabaseAccessMode::ActiveRecord => generate_active_record(sqir, params, wp),
    }
}

fn generate_pod(_sqir: &Sqir, _params: &CodegenParams, _wp: &mut WriterProvider) -> Result<()> {
    unimplemented!()
}

fn generate_active_record(_sqir: &Sqir, _params: &CodegenParams, _wp: &mut WriterProvider) -> Result<()> {
    unimplemented!()
}
