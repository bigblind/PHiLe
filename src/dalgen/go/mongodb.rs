//
// dalgen/go/mongodb.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 18/07/2017
//

use std::io;
use error::Result;
use dalgen::go::NAMING_CONVENTION;
use codegen::*;
use sqir::*;
use util::*;


pub fn generate_schema(_sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    let file_name = NAMING_CONVENTION.top_basename.to_owned() + ".go";
    let wptr = wp(&file_name)?;
    let mut wr = wptr.try_borrow_mut()?;
    let package_name = params.namespace.as_ref().map(
        |ns| transform_namespace(ns, params)
    ).ok_or_else(
        || io::Error::new(io::ErrorKind::InvalidInput, "Missing namespace")
    )?;

    write!(
        &mut *wr,
        include_str!("mongodb_template.txt"),
        version = PACKAGE_INFO.version,
        authors = PACKAGE_INFO.authors,
        ctxname = NAMING_CONVENTION.context_name,
        ctxtype = NAMING_CONVENTION.context_type,
        namespace = package_name,
    )?;

    Ok(())
}
