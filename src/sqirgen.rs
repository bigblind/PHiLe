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
    message: String,
    range:   Option<Range>,
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
    pub fn new() -> SQIRGen<'a> {
        SQIRGen {
            sqir: SQIR::new(),
        }
    }

    pub fn type_by_name(&self, name: &str) -> Option<&Type> {
        self.sqir.named_types.get(name)
    }

    pub fn generate_sqir(&mut self, node: &Node) -> SemaResult<&SQIR> {
        let children = match node.value {
            NodeValue::Program(children) => children,
            _ => return sema_error("top-level node must be a Program".to_owned(), node),
        };

        // Forward declare every struct/class/enum definition
        // by inserting a placeholder type for each of them
        for child in &children {
            let name = match child.value {
                NodeValue::StructDecl(s) => s.name,
                NodeValue::ClassDecl(c)  => c.name,
                NodeValue::EnumDecl(e)   => e.name,
                _ => continue,
            };

            self.sqir.named_types.insert(name, Type::PlaceholderType(name));
        }

        // Create semantic types out of AST and check their consistency
        for child in &children {
            match child.value {
                NodeValue::StructDecl(_) => self.declare_struct_type(child),
                NodeValue::ClassDecl(_)  => self.declare_class_type(child),
                NodeValue::EnumDecl(_)   => self.declare_enum_type(child),
                _ => continue,
            };
        }

        // Perform the occurs check on every user-defined type.
        // (It is only now that occurs checking is possible,
        // because at this point, we should have gotten rid of
        // all placeholders that could hide self-containing types.)
        for (name, tp) in self.sqir.named_types {
            // TODO(H2CO3): call occurs check
        }

        // Forward declare functions. (We can do this because we now have types.)
        // For each function: typecheck, and insert placeholder Function
        // into self.sqir.functions that has no actual body/implementation,
        // no instructions/basic blocks, only a type and argument names
        for child in &children {
            match child.value {
                NodeValue::FunctionDecl(f) => self.forward_declare_function(&f)?,
                _ => continue,
            }
        }

        // Then generate SQIR for each function
        for child in &children {
            match child.value {
                NodeValue::FunctionDecl(f) => self.generate_sqir_for_function(&f)?,
                _ => continue,
            }
        }

        // Finally, return the generated SQIR
        Ok(&self.sqir)
    }

    fn declare_struct_type(&mut self, node: &'a Node<'a>) -> SemaResult<&Type> {
        let decl = match node.value {
            NodeValue::StructDecl(ref decl) => decl,
            _ => return sema_error("declare_struct_type() requires a StructDecl node".to_owned(), node),
        };

        let name = decl.name;

        if self.named_types.contains_key(name) {
            return sema_error(format!("redefinition of '{}'", name), node);
        }

        // Insert a placeholder type so that we can typecheck fields
        self.named_types.insert(name, Type::PlaceholderType(name));

        let struct_type = Type::StructType(
            StructType {
                name:   name,
                fields: self.typecheck_struct_fields(decl)?,
            }
        );

        self.named_types.insert(name, struct_type);

        Ok(&self.named_types[name])
    }

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

            self.validate_field_type(field_type, node, decl.name)?;

            if fields.insert(field.name, field_type).is_some() {
                return sema_error(format!("duplicate field '{}'", field.name), node);
            }
        }

        Ok(fields)
    }

    fn declare_enum_type(&mut self, decl: &'a Node<'a>) -> SemaResult<&Type> {
        unimplemented!()
    }

    fn declare_class_type(&mut self, decl: &'a Node<'a>) -> SemaResult<&Type> {
        unimplemented!()
    }

    fn occurs_check(&mut self, ud_type: &Type) -> SemaResult<()> {
        unimplemented!()
    }

    fn type_from_decl(&mut self, decl: &Node) -> SemaResult<&'a Type<'a>> {
        unimplemented!()
    }

    fn validate_field_type(&mut self, field_type: &Type, field_node: &Node) -> SemaResult<()> {
        // No pointers are allowed in a struct.
        // Arrays, optionals and uniques are checked for
        // explicitly and recursively, because they are
        // not like user-defined enums or structs in that
        // they might legitimately contain pointers when
        // contained within a class.
        match field_type {
            Type::PointerType(t)  => sema_error("pointer type not allowed in struct".to_owned(), field_node),
            Type::OptionalType(t) => self.validate_field_type(t, field_node),
            Type::UniqueType(t)   => self.validate_field_type(t, field_node),
            Type::ArrayType(t)    => self.validate_field_type(t, field_type),
            Type::TupleType(ts)   => ts.map(|t| self.validate_field_type(t, field_node)).collect(),
            Type::EnumType(t)     => unimplemented!(), // TODO(H2CO3): look through each variant
            Type::StructType(t)   => unimplemented!(), // TODO(H2CO3): look through each field,
            Type::ClassType(t)    => sema_error(format!("class type {} not allowed in struct", t.name), field_node),
            _               => Ok(()), // atomic types (numbers, strings, blobs and dates) and placeholders are OK
        }
    }

    fn generate_sqir_for_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }

    fn forward_declare_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }
}
