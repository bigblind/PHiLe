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

pub fn generate_active_record(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    access_mode_error(params)
}

//
// The Actual Generator
//

impl<'a> Generator<'a> {
    fn generate_pod(mut self) -> io::Result<()> {
        // TODO(H2CO3): respect output file prefix
        // TODO(H2CO3): respect output directory
        // TODO(H2CO3): one file per user-defined type
        let wptr = (self.wp)("PHiLe_decls.go");
        let mut wr = wptr.try_borrow_mut().map_err(err)?;

        self.write_package_header(&mut *wr)?;
        self.write_driver_imports(&mut *wr)?;
        self.write_stdlib_imports(&mut *wr)?;

        // For determinism, sort user-defined types by name
        let types = self.types_sorted_by_name();

        for typ in &types {
            match *typ.borrow().map_err(err)? {
                Type::Struct(ref st) => self.write_fields(&mut *wr, &st.name, &st.fields)?,
                Type::Class(ref ct)  => self.write_fields(&mut *wr, &ct.name, &ct.fields)?,
                Type::Enum(ref et)   => self.write_variants(&mut *wr, &et.name, &et.variants)?,
                _ => continue, // primitive named types need no declaration
            }
        }

        Ok(())
    }

    // TODO(H2CO3): refactor
    fn write_fields(
        &self,
        wr: &mut io::Write,
        raw_struct_name: &str,
        fields: &HashMap<String, WkCell<Type>>
    ) -> io::Result<()> {
        // Respect the type name transform
        let struct_name = transform_type_name(
            raw_struct_name,
            self.params.type_name_transform,
            self.params.language
        );
        write!(wr, "\ntype {} struct {{\n", struct_name)?;

        // For determinism, sort fields by name.
        // Respect the field name transform too.
        // TODO(H2CO3): For efficiency, sort fields by alignment.
        let ordered_fields = {
            let mut fs: Vec<_> = fields.iter().map(|(fname, typ)| {
                let field_name = transform_field_name(
                    fname,
                    self.params.field_name_transform,
                    self.params.language
                );
                (field_name, typ)
            }).collect();

            fs.sort_by_key(|&(ref name, _)| name.clone());

            fs
        };

        let maxlen = ordered_fields.iter().map(|&(ref name, _)| name.len()).max().unwrap_or(0);
        let pad = " ".repeat(maxlen);

        for (fname, ftype) in ordered_fields {
            write!(wr, "    {}{} ", fname, &pad[fname.len()..])?;
            write_type(wr, ftype)?;
            write!(wr, "\n")?;
        }

        write!(wr, "}}\n")?;

        Ok(())
    }

    // TODO(H2CO3): refactor
    fn write_variants(
        &self,
        wr: &mut io::Write,
        raw_enum_name: &str,
        variants: &HashMap<String, WkCell<Type>>
    ) -> io::Result<()> {
        // Respect the type name transform
        let enum_name = transform_type_name(
            raw_enum_name,
            self.params.type_name_transform,
            self.params.language
        );

        // TODO(H2CO3): if none of the variants has an
        // associated value (every variant has type ()),
        // then just define the enum type as `string`.
        // TODO(H2CO3): let the user choose whether
        // enum discriminators are strings or ints, for
        // a tradeoff between speed and maintainability.
        write!(wr, "\ntype {} struct {{\n", enum_name)?;
        write!(wr, "    Variant string\n")?;
        write!(wr, "    Value   interface{{}}\n")?;
        write!(wr, "}}\n")?;

        // For determinism, sort variants by name.
        // Respect the variant name transform too.
        let ordered_variants = {
            let mut vs: Vec<_> = variants.keys().map(
                |vname| transform_variant_name(
                    vname,
                    self.params.variant_name_transform,
                    self.params.language
                )
            ).collect();

            vs.sort();

            vs
        };

        let maxlen = ordered_variants.iter().map(String::len).max().unwrap_or(0);
        let pad = " ".repeat(maxlen);

        write!(wr, "\nconst (\n")?;

        for vname in ordered_variants {
            let full_vname = enum_name.clone() + &vname;
            write!(wr, "    {}{} = \"{}\"\n", full_vname, &pad[vname.len()..], full_vname)?;
        }

        write!(wr, ")\n")?;

        Ok(())
    }

    //
    // Common Helpers
    //

    fn types_sorted_by_name(&self) -> Vec<&RcCell<Type>> {
        let mut types: Vec<_> = self.sqir.named_types.iter().collect();
        types.sort_by_key(|&(name, _)| name);
        types.iter().map(|&(_, typ)| typ).collect()
    }

    fn write_package_header(&self, wr: &mut io::Write) -> io::Result<()> {
        match self.params.namespace {
            Some(ref namespace) => {
                // Respect namespace transform
                let namespace_name = transform_namespace(
                    namespace,
                    self.params.namespace_transform,
                    self.params.language
                );
                write!(wr, "package {}\n\n", namespace_name)
            },
            None => Err(io::Error::new(io::ErrorKind::InvalidInput, "Missing namespace")),
        }
    }

    fn write_driver_imports(&self, wr: &mut io::Write) -> io::Result<()> {
        match self.params.database {
            DatabaseEngine::MongoDB => write!(wr, "import \"gopkg.in/mgo.v2\"\n"),
            DatabaseEngine::MariaDB => unimplemented!(),
            DatabaseEngine::SQLite3 => unimplemented!(),
        }
    }

    fn write_stdlib_imports(&self, wr: &mut io::Write) -> io::Result<()> {
        write!(wr, "import \"time\"\n")
    }
}

//
// Free Functions
//

fn err<E: Debug>(error: E) -> io::Error {
    io::Error::new(io::ErrorKind::Other, format!("{:#?}", error))
}

fn write_type(wr: &mut io::Write, typ: &WkCell<Type>) -> io::Result<()> {
    let rc = typ.as_rc().map_err(err)?;
    let ptr = rc.borrow().map_err(err)?;

    match *ptr {
        Type::Bool  => write!(wr, "bool"),
        Type::Int   => write!(wr, "int64"),
        Type::Float => write!(wr, "float64"),
        Type::Decimal(integral, fractional) => unimplemented!(),

        Type::String => write!(wr, "string"),
        Type::Blob   => write!(wr, "[]byte"),
        Type::Date   => write!(wr, "time.Time"),

        Type::Optional(ref wrapped) => write_optional_type(wr, wrapped),
        Type::Unique(ref wrapped)   => write_type(wr, wrapped), // don't care
        Type::Pointer(ref pointed)  => write_pointer_type(wr, pointed),
        Type::Array(ref element)    => write_array_type(wr, element),
        Type::Tuple(ref types)      => write_tuple_type(wr, types),

        // TODO(H2CO3): respect type name transform
        Type::Enum(ref et)   => write!(wr, "{}", et.name),
        Type::Struct(ref st) => write!(wr, "{}", st.name),
        Type::Class(ref ct)  => write!(wr, "{}", ct.name),

        Type::Function(ref ft) => unimplemented!(),
        Type::Placeholder(ref name, kind) => unreachable!("Unresolved Placeholder({}, {:#?})", name, kind),
    }
}

fn write_optional_type(wr: &mut io::Write, wrapped: &WkCell<Type>) -> io::Result<()> {
    write_pointer_type(wr, wrapped)
}

fn write_pointer_type(wr: &mut io::Write, pointed: &WkCell<Type>) -> io::Result<()> {
    write!(wr, "*").and_then(|_| write_type(wr, pointed))
}

fn write_array_type(wr: &mut io::Write, element: &WkCell<Type>) -> io::Result<()> {
    write!(wr, "[]").and_then(|_| write_type(wr, element))
}

fn write_tuple_type(wr: &mut io::Write, types: &[WkCell<Type>]) -> io::Result<()> {
    write!(wr, "struct {{ ")?;

    for (idx, typ) in types.iter().enumerate() {
        write!(wr, "Field{} ", idx)?;
        write_type(wr, typ)?;
        write!(wr, "; ")?;
    }

    write!(wr, "}}")?;

    Ok(())
}
