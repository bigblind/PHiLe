//
// declgen/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

use std::io;
use std::fmt::Debug;
use codegen::*;
use sqir::*;


pub fn generate_pod(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    fn err<E: Debug>(error: E) -> io::Error {
        io::Error::new(io::ErrorKind::Other, format!("{:#?}", error))
    }

    let fptr = wp("PHiLe_decls.go");
    let file = fptr.try_borrow_mut().map_err(err)?;

    for (name, typ) in &sqir.named_types {
        match *typ.borrow().map_err(err)? {
            Type::Struct(ref st) => (),
            Type::Class(ref ct) => (),
            _ => (),
        }
    }

    Ok(())
}

pub fn generate_active_record(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    access_mode_error(params)
}
