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
use util::PACKAGE_INFO;
use declgen::go::NAMING_CONVENTION;


pub fn generate(_sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    let file_name = TOPLEVEL_BASENAME.to_owned() + ".go";
    let wptr = wp(&file_name)?;
    let mut wr = wptr.borrow_mut();
    let package_name = params.namespace.as_ref().map(
        |ns| transform_namespace(ns, params)
    ).ok_or_else(
        || io::Error::new(io::ErrorKind::InvalidInput, "Missing namespace")
    )?;

    write!(
        &mut *wr,
        include_str!("go_template.txt"),
        version = PACKAGE_INFO.version,
        authors = PACKAGE_INFO.authors,
        ctxtype = NAMING_CONVENTION.context_type,
        namespace = package_name,
    )
}
