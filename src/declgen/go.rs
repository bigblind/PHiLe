//
// declgen/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

use std::io;
use std::fmt::Debug;
use std::collections::HashMap;
use codegen::*;
use sqir::*;
use util::*;


pub fn generate_pod(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    let wptr = wp("PHiLe_decls.go");
    let mut wr = wptr.try_borrow_mut().map_err(err)?;

    write!(wr, "package main\n\nimport \"time\"\n\n")?;

    // TODO(H2eCO3): for determinism, sort named
    // user-defined types by name
    for (name, typ) in &sqir.named_types {
        match *typ.borrow().map_err(err)? {
            Type::Struct(ref st) => declare_fields(&st.name, &st.fields, &mut *wr)?,
            Type::Class(ref ct) => declare_fields(&ct.name, &ct.fields, &mut *wr)?,
            _ => continue,
        }
    }

    write!(wr, "func main() {{\n}}\n")?;

    Ok(())
}

fn declare_fields(name: &str, fields: &HashMap<String, WkCell<Type>>, wr: &mut io::Write) -> io::Result<()> {
    // TODO(H2CO3): respect the type name transform
    write!(wr, "type {} struct {{\n", name)?;

    // TODO(H2CO3): Respect the field name transform
    // TODO(H2CO3): For determinism and efficiency,
    // sort fields by alignment and then by name.
    match fields.keys().map(String::len).max() {
        None => (),
        Some(maxlen) => {
            let pad = " ".repeat(maxlen);
            for (fname, ftype) in fields {
                write!(wr, "    {}{} ", fname, &pad[fname.len()..])?;
                write_type(wr, ftype);
                write!(wr, "\n")?;
            }
        }
    }

    write!(wr, "}}\n\n")?;

    Ok(())
}

fn write_type(wr: &mut io::Write, typ: &WkCell<Type>) -> io::Result<()> {
    let rc = typ.as_rc().map_err(err)?;
    let ptr = rc.borrow().map_err(err)?;

    match *ptr {
        Type::Bool => write!(wr, "bool"),
        Type::Int => write!(wr, "int64"),
        Type::Float => write!(wr, "float64"),
        Type::Decimal(integral, fractional) => unimplemented!(),

        Type::String => write!(wr, "string"),
        Type::Blob => write!(wr, "[]byte"),
        Type::Date => write!(wr, "time.Time"),

        Type::Optional(ref wrapped) => unimplemented!(),
        Type::Unique(ref wrapped) => unimplemented!(),
        Type::Pointer(ref pointed) => unimplemented!(),
        Type::Array(ref element) => unimplemented!(),
        Type::Tuple(ref types) => unimplemented!(),

        // TODO(H2CO3): respect the type name transform
        Type::Enum(ref et) => write!(wr, "{}", et.name),
        Type::Struct(ref st) => write!(wr, "{}", st.name),
        Type::Class(ref ct) => write!(wr, "{}", ct.name),

        Type::Function(ref ft) => unimplemented!(),

        Type::Placeholder(ref name, kind) => unreachable!("Unresolved Placeholder({}, {:#?})", name, kind),
    }
}

//
// Active Record database access mode (not supported yet)
//

pub fn generate_active_record(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    access_mode_error(params)
}

//
// General Helpers
//

fn err<E: Debug>(error: E) -> io::Error {
    io::Error::new(io::ErrorKind::Other, format!("{:#?}", error))
}
