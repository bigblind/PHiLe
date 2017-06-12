//
// schemagen/mongodb/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 08/06/2017
//

use std::io;
use codegen::*;
use sqir::*;


pub fn generate(_sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    let wptr = wp("PHiLe-Context.go")?;
    let mut wr = wptr.borrow_mut();
    let package_name = params.namespace.as_ref().map(
        |ns| transform_namespace(ns, params)
    ).ok_or_else(
        || io::Error::new(io::ErrorKind::InvalidInput, "Missing namespace")
    )?;

    write!(
        &mut *wr,
        include_str!("go_template.txt"),
        version = env!["CARGO_PKG_VERSION"],
        authors = env!["CARGO_PKG_AUTHORS"],
        namespace = package_name
    )
}
