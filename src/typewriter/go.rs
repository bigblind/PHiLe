//
// typewriter/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 12/07/2017
//

use std::io;
use codegen::*;
use sqir::*;


struct TypeWriter<'a> {
    params: &'a CodegenParams,
}


pub fn write_type(wr: &mut io::Write, ty: &WkType, params: &CodegenParams) -> io::Result<()> {
    TypeWriter { params }.write_type(wr, ty)
}

impl<'a> TypeWriter<'a> {
    fn write_type(&self, wr: &mut io::Write, ty: &WkType) -> io::Result<()> {
        let rc = ty.as_rc()?;
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
            write!(wr, "F{} ", idx)?;
            self.write_type(wr, typ)?;
            write!(wr, "; ")?;
        }

        write!(wr, "}}")?;

        Ok(())
    }
}
