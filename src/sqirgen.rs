//
// sqirgen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::collections::HashMap;
use sqir::*;
use lexer::*;
use ast::{ Node, NodeValue, EnumDecl, StructDecl, ClassDecl, FunctionDecl };


#[derive(Debug, Clone)]
pub struct SemaError {
    pub message: String,
    pub range:   Option<Range>,
}

#[allow(missing_debug_implementations)]
pub struct SQIRGen<'a> {
    sqir: SQIR<'a>,
}

pub type SemaResult<T> = Result<T, SemaError>;


fn sema_error<T>(message: String, node: &Node) -> SemaResult<T> {
    Err(
        SemaError {
            message: message,
            range:   node.range,
        }
    )
}

impl<'a> SQIRGen<'a> {
    // Constructor
    pub fn new() -> SQIRGen<'a> {
        SQIRGen {
            sqir: SQIR::new(),
        }
    }

    //
    // Top-level SQIR generation methods and helpers
    //

    pub fn generate_sqir(mut self, node: &'a Node<'a>) -> SemaResult<SQIR<'a>> {
        let children = match node.value {
            NodeValue::Program(ref children) => children,
            _ => return sema_error("top-level node must be a Program".to_owned(), node),
        };

        self.forward_declare_user_defined_types(children)?;
        self.define_user_defined_types(children)?;
        self.occurs_check_user_defined_types()?;
        self.forward_declare_functions(children)?;
        self.generate_functions(children)?;

        Ok(self.sqir)
    }

    fn forward_declare_user_defined_types(&mut self, children: &Vec<Node<'a>>) -> SemaResult<()> {
        // Forward declare every struct/class/enum definition
        // by inserting a placeholder type for each of them
        for child in children {
            let name = match child.value {
                NodeValue::StructDecl(ref s) => s.name,
                NodeValue::ClassDecl(ref c)  => c.name,
                NodeValue::EnumDecl(ref e)   => e.name,
                _ => continue,
            };

            if self.sqir.named_types.contains_key(name) {
                return sema_error(format!("redefinition of '{}'", name), child);
            }

            self.sqir.named_types.insert(name, Type::PlaceholderType(name));
        }

        Ok(())
    }

    fn define_user_defined_types(&mut self, children: &'a Vec<Node<'a>>) -> SemaResult<()> {
        // Create semantic types out of AST and check their consistency
        for child in children {
            match child.value {
                NodeValue::StructDecl(ref s) => self.define_struct_type(s)?,
                NodeValue::ClassDecl(ref c)  => self.define_class_type(c)?,
                NodeValue::EnumDecl(ref e)   => self.define_enum_type(e)?,
                _ => continue,
            };
        }

        Ok(())
    }

    fn occurs_check_user_defined_types(&self) -> SemaResult<()> {
        // Perform the occurs check on every user-defined type.
        // (It is only now that occurs checking is possible,
        // because at this point, we should have gotten rid of
        // all placeholders that could hide self-containing types.)
        for (_, t) in &self.sqir.named_types {
            self.occurs_check(t)?;
        }

        Ok(())
    }

    fn forward_declare_functions(&mut self, children: &Vec<Node>) -> SemaResult<()> {
        // Forward declare functions. (We can do this because we now have types.)
        // For each function: typecheck, and insert placeholder Function
        // into self.sqir.functions that has no actual body/implementation,
        // no instructions/basic blocks, only a type and argument names
        for child in children {
            match child.value {
                NodeValue::FunctionDecl(ref f) => self.forward_declare_function(&f)?,
                _ => continue,
            }
        }

        Ok(())
    }

    fn generate_functions(&mut self, children: &Vec<Node>) -> SemaResult<()> {
        // Generate SQIR for each function
        for child in children {
            match child.value {
                NodeValue::FunctionDecl(ref f) => self.generate_function(&f)?,
                _ => continue,
            }
        }

        Ok(())
    }

    //
    // Type-wise semantic analysis methods
    //

    fn define_struct_type(&mut self, decl: &'a StructDecl<'a>) -> SemaResult<&Type> {
        let name = decl.name;

        let struct_type = Type::StructType(
            StructType {
                name:   name,
                fields: self.typecheck_struct_fields(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        self.sqir.named_types.insert(name, struct_type);

        Ok(&self.sqir.named_types[name])
    }

    fn define_enum_type(&mut self, decl: &'a EnumDecl<'a>) -> SemaResult<&Type> {
        unimplemented!()
    }

    fn define_class_type(&mut self, decl: &'a ClassDecl<'a>) -> SemaResult<&Type> {
        unimplemented!()
    }

    //
    // Helpers for struct types
    //

    fn typecheck_struct_fields(&mut self, decl: &'a StructDecl) -> SemaResult<HashMap<&'a str, &'a Type<'a>>> {
        let mut fields = HashMap::with_capacity(decl.fields.len());

        for node in &decl.fields {
            let field = match node.value {
                NodeValue::Field(ref field) => field,
                _ => return sema_error("struct fields must be Field values".to_owned(), node),
            };

            // No relations are allowed in a struct.
            if field.relation.is_some() {
                return sema_error(format!("struct field '{}' must not be part of a relation", field.name), node);
            }

            // Consequently, types cannot be inferred,
            // so type annotations are obligatory.
            let type_decl = match field.type_decl {
                Some(ref type_decl) => type_decl,
                _ => return sema_error(format!("field '{}' must have a type annotation", field.name), node),
            };

            let field_type = self.type_from_decl(&type_decl)?;

            self.validate_struct_field_type(field_type, node)?;

            if fields.insert(field.name, field_type).is_some() {
                return sema_error(format!("duplicate field '{}'", field.name), node);
            }
        }

        Ok(fields)
    }

    fn validate_struct_field_type(&self, field_type: &Type, field_node: &Node) -> SemaResult<()> {
        match *field_type {
            // No pointers (and consequently, no classes) are allowed in a struct.
            Type::PointerType(_) => sema_error("pointer type not allowed in struct".to_owned(), field_node),
            Type::ClassType(ref t) => sema_error(format!("class type {} not allowed in struct", t.name), field_node),

            // Optionals, uniques, and arrays are checked for
            // explicitly and recursively, because they are
            // not like user-defined enums or structs in that
            // they might legitimately contain pointers when
            // contained within a class.
            Type::OptionalType(ref t) => self.validate_struct_field_type(t, field_node),
            Type::UniqueType(ref t)   => self.validate_struct_field_type(t, field_node),
            Type::ArrayType(ref t)    => self.validate_struct_field_type(t, field_node),

            // Every type of a contained tuple, every member of
            // a contained struct, and every variant of a contained enum
            // must be valid as well.
            Type::TupleType(ref types) => {
                types.iter().map(
                    |t| self.validate_struct_field_type(t, field_node)
                ).collect::<SemaResult<Vec<_>>>().and(Ok(()))
            },
            Type::StructType(ref st) => {
                st.fields.iter().map(
                    |(_, t)| self.validate_struct_field_type(t, field_node)
                ).collect::<SemaResult<Vec<_>>>().and(Ok(()))
            },
            Type::EnumType(ref et) => {
                et.variants.iter().map(
                    |v| v.types.iter().map(
                        |t| self.validate_struct_field_type(t, field_node)
                    ).collect::<SemaResult<Vec<_>>>()
                ).collect::<SemaResult<Vec<_>>>().and(Ok(()))
            },

            // atomic types (numbers, strings, blobs, and dates) and placeholders are OK
            _ => Ok(()),
        }
    }

    //
    // Helpers for enum types
    //

    //
    // Helpers for class types
    //

    // Occurs Check
    fn occurs_check(&self, ud_type: &Type) -> SemaResult<()> {
        unimplemented!()
    }

    //
    // Function-level SQIR generation
    //

    fn forward_declare_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }

    fn generate_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }

    //
    // Miscellaneous helpers
    //

    fn type_from_decl(&mut self, decl: &Node) -> SemaResult<&'a Type<'a>> {
        unimplemented!()
    }

    fn type_by_name(&self, name: &str) -> Option<&Type> {
        self.sqir.named_types.get(name)
    }
}
