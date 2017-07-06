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
use codegen::*;
use sqir::*;
use util::*;


struct Generator<'a> {
    sqir:   &'a SQIR,
    params: &'a CodegenParams,
    wp:     &'a mut WriterProvider,
}


pub fn generate_pod(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    Generator { sqir, params, wp }.generate_pod()
}

//
// Active Record database access mode (not yet supported)
//

pub fn generate_active_record(_sqir: &SQIR, _params: &CodegenParams, _wp: &mut WriterProvider) -> io::Result<()> {
    unimplemented!()
}

//
// The Actual Generator
//

impl<'a> Generator<'a> {
    fn generate_pod(mut self) -> io::Result<()> {
        let wrs = self.writers_for_types(self.sqir.named_types.iter())?;

        for (name, typ) in &self.sqir.named_types {
            let wr = &mut *wrs[name].borrow_mut();

            match *typ.borrow()? {
                Type::Struct(ref st) => self.write_fields(wr, name, &st.fields)?,
                Type::Class(ref ct)  => self.write_fields(wr, name, &ct.fields)?,
                Type::Enum(ref et)   => self.write_variants(wr, name, &et.variants)?,
                _ => continue, // primitive named types need no declaration
            }
        }

        Ok(())
    }

    fn writers_for_types<'b, I>(&mut self, types: I) -> io::Result<BTreeMap<String, Rc<RefCell<io::Write>>>>
        where I: Iterator<Item=(&'b String, &'b RcType)> {

        let mut writers = BTreeMap::new();

        for (name, typ) in types {
            // primitive named types need no declaration
            match *typ.borrow()? {
                Type::Struct(_) | Type::Class(_) | Type::Enum(_) => (),
                _ => continue,
            }

            writers.insert(name.clone(), self.writer_with_preamble(name)?);
        }

        Ok(writers)
    }

    fn writer_with_preamble(&mut self, name: &str) -> io::Result<Rc<RefCell<io::Write>>> {
        let file_name = name.to_owned() + ".go";
        let wptr = (self.wp)(&file_name)?;
        self.write_header(&mut *wptr.borrow_mut())?;
        Ok(wptr)
    }

    // TODO(H2CO3): refactor
    fn write_fields(
        &self,
        wr: &mut io::Write,
        raw_struct_name: &str,
        fields: &BTreeMap<String, WkType>,
    ) -> io::Result<()> {
        // Respect the type name transform
        let struct_name = transform_type_name(raw_struct_name, &self.params);
        writeln!(wr, "type {} struct {{", struct_name)?;

        // Respect the field name transform
        // TODO(H2CO3): For efficiency, sort fields by alignment.
        let transformed_fields: Vec<_> = fields.iter().map(|(fname, typ)| {
            let field_name = transform_field_name(fname, &self.params);
            (field_name, typ)
        }).collect();

        let max_len = transformed_fields.iter()
            .map(|&(ref name, _)| grapheme_count(name))
            .max().unwrap_or(0);

        let pad = " ".repeat(max_len);

        // Indexing `pad` with grapheme counts is safe because it's ASCII-only.
        for (fname, ftype) in transformed_fields {
            write!(wr, "    {}{} ", fname, &pad[grapheme_count(&fname)..])?;
            self.write_type(wr, ftype)?;
            writeln!(wr)?;
        }

        writeln!(wr, "}}\n")?;

        Ok(())
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

        writeln!(wr, ")\n")?;

        Ok(())
    }

    //
    // Generic Type Writers
    //
    fn write_type(&self, wr: &mut io::Write, typ: &WkType) -> io::Result<()> {
        let rc = typ.as_rc()?;
        let ptr = rc.borrow()?;

        match *ptr {
            Type::Bool  => write!(wr, "bool"),
            Type::Int   => write!(wr, "int64"),
            Type::Float => write!(wr, "float64"),
            Type::Decimal { .. } => unimplemented!(),

            Type::String => write!(wr, "string"),
            Type::Blob   => write!(wr, "[]byte"),
            Type::Date   => write!(wr, "time.Time"),

            Type::Optional(ref wrapped) => self.write_optional_type(wr, wrapped),
            Type::Pointer(ref pointed)  => self.write_pointer_type(wr, pointed),
            Type::Array(ref element)    => self.write_array_type(wr, element),
            Type::Tuple(ref types)      => self.write_tuple_type(wr, types),

            // Respect type name transform
            Type::Enum(ref et)   => write!(wr, "{}", transform_type_name(&et.name, self.params)),
            Type::Struct(ref st) => write!(wr, "{}", transform_type_name(&st.name, self.params)),
            Type::Class(ref ct)  => write!(wr, "{}", transform_type_name(&ct.name, self.params)),

            Type::Function(_) => unimplemented!(),
            Type::Placeholder { ref name, kind } => unreachable!("Unresolved Placeholder({}, {:#?})", name, kind),
        }
    }

    fn write_optional_type(&self, wr: &mut io::Write, wrapped: &WkType) -> io::Result<()> {
        self.write_pointer_type(wr, wrapped)
    }

    fn write_pointer_type(&self, wr: &mut io::Write, pointed: &WkType) -> io::Result<()> {
        write!(wr, "*").and_then(|_| self.write_type(wr, pointed))
    }

    fn write_array_type(&self, wr: &mut io::Write, element: &WkType) -> io::Result<()> {
        write!(wr, "[]").and_then(|_| self.write_type(wr, element))
    }

    fn write_tuple_type(&self, wr: &mut io::Write, types: &[WkType]) -> io::Result<()> {
        write!(wr, "struct {{ ")?;

        for (idx, typ) in types.iter().enumerate() {
            write!(wr, "Field{} ", idx)?;
            self.write_type(wr, typ)?;
            write!(wr, "; ")?;
        }

        write!(wr, "}}")?;

        Ok(())
    }

    //
    // Common Helpers
    //

    fn write_header(&self, wr: &mut io::Write) -> io::Result<()> {
        self.write_comment_header(wr)?;
        self.write_namespace(wr)?;
        self.write_imports(wr)?;
        self.write_dummy_uses(wr)?;
        Ok(())
    }

    fn write_comment_header(&self, wr: &mut io::Write) -> io::Result<()> {
        writeln!(wr, "//")?;
        writeln!(wr, "// Generated by PHiLe v{}", env!["CARGO_PKG_VERSION"])?;
        writeln!(wr, "// Copyright (C) 2017, {}", env!["CARGO_PKG_AUTHORS"])?;
        writeln!(wr, "//")?;
        writeln!(wr)
    }

    // Namespace <-> package name
    // Respect namespace transform
    fn write_namespace(&self, wr: &mut io::Write) -> io::Result<()> {
        let package_name = self.params.namespace.as_ref().map(
            |ns| transform_namespace(ns, &self.params)
        ).ok_or_else(
            || io::Error::new(io::ErrorKind::InvalidInput, "Missing namespace")
        )?;

        writeln!(wr, "package {}\n", package_name)
    }

    fn write_imports(&self, wr: &mut io::Write) -> io::Result<()> {
        // stdlib imports
        writeln!(wr, "import \"time\"")?;

        // driver-specific imports
        match self.params.database {
            DatabaseEngine::MongoDB => writeln!(wr, "import \"gopkg.in/mgo.v2\"")?,
            DatabaseEngine::MariaDB => unimplemented!(),
            DatabaseEngine::SQLite3 => unimplemented!(),
        }

        writeln!(wr)
    }

    fn write_dummy_uses(&self, wr: &mut io::Write) -> io::Result<()> {
        // stdlib stuff is always imported and should be used
        writeln!(wr, "type _ time.Time")?;

        // driver imports are conditional, so are their dummy uses
        match self.params.database {
            DatabaseEngine::MongoDB => writeln!(wr, "type _ mgo.Session")?,
            DatabaseEngine::MariaDB => unimplemented!(),
            DatabaseEngine::SQLite3 => unimplemented!(),
        }

        writeln!(wr)
    }
}
