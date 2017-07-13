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
use typewriter::go::*;
use codegen::*;
use sqir::*;
use util::*;


struct Generator<'a> {
    sqir:   &'a SQIR,
    params: &'a CodegenParams,
    wp:     &'a mut WriterProvider,
}


//
// Plain Old Data access mode
//

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
            // primitive named types have no associated writer
            let mut wr = match wrs.get(name) {
                Some(w) => w.borrow_mut(),
                None    => continue,
            };

            match *typ.borrow()? {
                Type::Struct(ref st) => self.write_fields(&mut *wr, name, &st.fields)?,
                Type::Class(ref ct)  => self.write_fields(&mut *wr, name, &ct.fields)?,
                Type::Enum(ref et)   => self.write_variants(&mut *wr, name, &et.variants)?,
                _ => unreachable!("Attempt to declare primitive type?!"),
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
        write_header(&mut *wptr.borrow_mut(), &self.params)?;
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
            write_type(wr, ftype, &self.params)?;
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
}
