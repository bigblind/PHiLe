//
// declgen/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

use std::io;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use error::{ Error, Result };
use codegen::*;
use sqir::*;
use util::*;


//
// Naming Conventions for frequently-occurring types and variables
//

#[derive(Debug, Clone, Copy)]
pub struct NamingConvention {
    pub context_type: &'static str,
    pub context_name: &'static str,
    pub tmp_prefix:   &'static str,
    pub var_prefix:   &'static str,
    pub top_basename: &'static str,
}

pub static NAMING_CONVENTION: NamingConvention = NamingConvention {
    context_type: "PhileCtx",
    context_name: "ctx",
    tmp_prefix:   "tmp_",
    var_prefix:   "var_",
    top_basename: "PHiLe-Context",
};

struct Generator<'a> {
    sqir:   &'a SQIR,
    params: &'a CodegenParams,
    wp:     &'a mut WriterProvider,
}


//
// Plain Old Data access mode
//

pub fn generate_pod(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    Generator { sqir, params, wp }.generate_pod()
}

//
// Active Record database access mode (not yet supported)
//

pub fn generate_active_record(_sqir: &SQIR, _params: &CodegenParams, _wp: &mut WriterProvider) -> Result<()> {
    unimplemented!()
}

//
// The Actual Generator
//

impl<'a> Generator<'a> {
    fn generate_pod(mut self) -> Result<()> {
        let wrs = self.writers_for_types(self.sqir.named_types.iter())?;

        for (name, ty) in &self.sqir.named_types {
            // primitive named types have no associated writer
            let mut wr = match wrs.get(name) {
                Some(w) => w.try_borrow_mut()?,
                None    => continue,
            };

            match *ty.borrow()? {
                Type::Struct(ref st) => self.write_fields(&mut *wr, name, &st.fields)?,
                Type::Class(ref ct)  => self.write_fields(&mut *wr, name, &ct.fields)?,
                Type::Enum(ref et)   => self.write_variants(&mut *wr, name, &et.variants)?,
                ref t => bug!("Attempt to declare primitive type {}?!", t),
            }
        }

        Ok(())
    }

    fn writers_for_types<'b, I>(&mut self, types: I) -> Result<BTreeMap<String, Rc<RefCell<io::Write>>>>
        where I: Iterator<Item=(&'b String, &'b RcType)> {

        let mut writers = BTreeMap::new();

        for (name, ty) in types {
            // primitive named types need no declaration
            match *ty.borrow()? {
                Type::Struct(_) | Type::Class(_) | Type::Enum(_) => (),
                _ => continue,
            }

            writers.insert(name.clone(), self.writer_with_preamble(name)?);
        }

        Ok(writers)
    }

    fn writer_with_preamble(&mut self, name: &str) -> Result<Rc<RefCell<io::Write>>> {
        let file_name = name.to_owned() + ".go";
        let wptr = (self.wp)(&file_name)?;
        write_header(&mut *wptr.try_borrow_mut()?, &self.params)?;
        Ok(wptr)
    }

    // TODO(H2CO3): refactor
    fn write_fields(
        &self,
        wr: &mut io::Write,
        raw_struct_name: &str,
        fields: &BTreeMap<String, WkType>,
    ) -> Result<()> {
        // Respect the type name transform
        let struct_name = transform_type_name(raw_struct_name, &self.params);
        writeln!(wr, "type {} struct {{", struct_name)?;

        // Respect the field name transform
        // TODO(H2CO3): For efficiency, sort fields by alignment.
        let transformed_fields: Vec<_> = fields.iter().map(
            |(name, ty)| (transform_field_name(name, &self.params), ty)
        ).collect();

        let max_len = transformed_fields.iter()
            .map(|&(ref name, _)| grapheme_count(name))
            .max().unwrap_or(0);

        let pad = " ".repeat(max_len);

        // Indexing `pad` with grapheme counts is safe because it's ASCII-only.
        for (name, ty) in transformed_fields {
            write!(wr, "    {}{} ", name, &pad[grapheme_count(&name)..])?;
            write_type(wr, ty, &self.params)?;
            writeln!(wr)?;
        }

        writeln!(wr, "}}\n").map_err(From::from)
    }

    // TODO(H2CO3): refactor
    fn write_variants(
        &self,
        wr: &mut io::Write,
        raw_enum_name: &str,
        variants: &BTreeMap<String, WkType>,
    ) -> io::Result<()> {
        // Respect the type name transform
        let enum_name = transform_type_name(raw_enum_name, &self.params);

        // TODO(H2CO3): if none of the variants has an
        // associated value (every variant has type ()),
        // then just define the enum type as `string`.
        // TODO(H2CO3): let the user choose whether
        // enum discriminators are strings or ints, for
        // a tradeoff between speed and maintainability.
        // TODO(H2CO3): transform these field names...?!
        writeln!(wr, "type {} struct {{", enum_name)?;
        writeln!(wr, "    Variant string")?;
        writeln!(wr, "    Value   interface{{}}")?;
        writeln!(wr, "}}\n")?;

        // Respect the variant name transform.
        // For correctly prefixing each variant name, first
        // they are prefixed with the name of the variant and
        // an underscore, which is a word boundary according
        // to the case transforms in the Heck crate. Then, the
        // composed 'raw' variant name is transformed to obtain
        // the final, correctly cased and prefixed variant name.
        let transformed_variants: Vec<_> = variants.keys().map(|vname| {
            let variant_name = raw_enum_name.to_owned() + "_" + vname;
            transform_variant_name(&variant_name, &self.params)
        }).collect();

        let max_len = transformed_variants.iter().map(String::len).max().unwrap_or(0);
        let pad = " ".repeat(max_len);

        writeln!(wr, "const (")?;

        for vname in &transformed_variants {
            writeln!(wr, "    {}{} = \"{}\"", vname, &pad[grapheme_count(vname)..], vname)?;
        }

        writeln!(wr, ")\n")
    }
}

//
// Type annotations for declarations, etc.
//

pub fn write_type(wr: &mut io::Write, ty: &WkType, params: &CodegenParams) -> Result<()> {
    let rc = ty.as_rc()?;
    let ptr = rc.borrow()?;

    match *ptr {
        Type::Bool  => write!(wr, "bool")?,
        Type::Int   => write!(wr, "int64")?,
        Type::Float => write!(wr, "float64")?,
        Type::Decimal { .. } => unimplemented!(),

        Type::String => write!(wr, "string")?,
        Type::Blob   => write!(wr, "[]byte")?,
        Type::Date   => write!(wr, "time.Time")?,

        Type::Optional(ref wrapped) => write_optional_type(wr, wrapped, params)?,
        Type::Pointer(ref pointed)  => write_pointer_type(wr, pointed, params)?,
        Type::Array(ref element)    => write_array_type(wr, element, params)?,
        Type::Tuple(ref types)      => write_tuple_type(wr, types, params)?,

        // Respect type name transform
        Type::Enum(ref et)   => write!(wr, "{}", transform_type_name(&et.name, params))?,
        Type::Struct(ref st) => write!(wr, "{}", transform_type_name(&st.name, params))?,
        Type::Class(ref ct)  => write!(wr, "{}", transform_type_name(&ct.name, params))?,

        Type::Function(ref ft) => write_function_type(wr, ft, params)?,
        Type::Placeholder { ref name, kind } => bug!("Unresolved Placeholder({}, {})", name, kind),
    }

    Ok(())
}

fn write_optional_type(wr: &mut io::Write, wrapped: &WkType, params: &CodegenParams) -> Result<()> {
    write_pointer_type(wr, wrapped, params)
}

fn write_pointer_type(wr: &mut io::Write, pointed: &WkType, params: &CodegenParams) -> Result<()> {
    write!(wr, "*").map_err(From::from).and_then(|_| write_type(wr, pointed, params))
}

fn write_array_type(wr: &mut io::Write, element: &WkType, params: &CodegenParams) -> Result<()> {
    write!(wr, "[]").map_err(From::from).and_then(|_| write_type(wr, element, params))
}

fn write_tuple_type(wr: &mut io::Write, types: &[WkType], params: &CodegenParams) -> Result<()> {
    write!(wr, "struct {{ ")?;

    for (idx, ty) in types.iter().enumerate() {
        write!(wr, "F{} ", idx)?;
        write_type(wr, ty, params)?;
        write!(wr, "; ")?;
    }

    write!(wr, "}}").map_err(From::from)
}

fn write_function_type(wr: &mut io::Write, ty: &FunctionType, params: &CodegenParams) -> Result<()> {
    write!(wr, "func({}", NAMING_CONVENTION.context_type)?;

    for arg in &ty.arg_types {
        write!(wr, ", ")?;
        write_type(wr, arg, params)?;
    }

    write!(wr, ") ")?;

    write_type(wr, &ty.ret_type, params).map_err(From::from)
}

//
// Package/namespace, include and dummy type uses
//

fn write_header(wr: &mut io::Write, params: &CodegenParams) -> Result<()> {
    write_comment_header(wr)?;
    write_namespace(wr, params)?;
    write_imports(wr, params)?;
    write_dummy_uses(wr, params)?;
    Ok(())
}

fn write_comment_header(wr: &mut io::Write) -> io::Result<()> {
    writeln!(wr, "//")?;
    writeln!(wr, "// Generated by PHiLe v{}", PACKAGE_INFO.version)?;
    writeln!(wr, "// Copyright (C) 2017, {}", PACKAGE_INFO.authors)?;
    writeln!(wr, "//")?;
    writeln!(wr)
}

// Namespace <-> package name
// Respect namespace transform
fn write_namespace(wr: &mut io::Write, params: &CodegenParams) -> Result<()> {
    params.namespace.as_ref().map(
        |ns| transform_namespace(ns, params)
    ).ok_or_else(|| Error::Semantic {
        message: "Missing namespace".to_owned(),
        range:   None,
    }).and_then(
        |ns| writeln!(wr, "package {}\n", ns).map_err(From::from)
    )
}

fn write_imports(wr: &mut io::Write, params: &CodegenParams) -> io::Result<()> {
    // stdlib imports
    writeln!(wr, "import \"time\"")?;

    // driver-specific imports
    match params.database {
        DatabaseEngine::MongoDB => writeln!(wr, "import \"gopkg.in/mgo.v2\"")?,
        DatabaseEngine::MariaDB => unimplemented!(),
        DatabaseEngine::SQLite3 => unimplemented!(),
    }

    writeln!(wr)
}

fn write_dummy_uses(wr: &mut io::Write, params: &CodegenParams) -> io::Result<()> {
    // stdlib stuff is always imported and should be used
    writeln!(wr, "type _ time.Time")?;

    // driver imports are conditional, so are their dummy uses
    match params.database {
        DatabaseEngine::MongoDB => writeln!(wr, "type _ mgo.Session")?,
        DatabaseEngine::MariaDB => unimplemented!(),
        DatabaseEngine::SQLite3 => unimplemented!(),
    }

    writeln!(wr)
}

//
// Escaping string literals
//

pub fn escape_string_literal(s: &str) -> String {
    // printable characters need not be escaped
    if s.chars().all(|c| 0x20 as char <= c && c <= 0x7e as char) {
        s.to_owned()
    } else {
        unimplemented!() // TODO(H2CO3): escape string literals; NB escape_default()!
    }
}
