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
use ast::{ Node, NodeValue, EnumDecl, StructDecl, ClassDecl };


#[derive(Debug, Clone)]
pub struct SemaError {
    message: String,
    range:   Option<Range>,
}

#[allow(missing_debug_implementations)]
pub struct QIRGen<'a> {
    named_types: HashMap<&'a str, Type<'a>>,
    relations:   Vec<Relation<'a>>,
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

impl<'a> QIRGen<'a> {
    pub fn new() -> QIRGen<'a> {
        QIRGen {
            named_types: hash_map![], // named_types,
            relations:   vec![], // relations,
        }
    }

    pub fn type_by_name(&self, name: &str) -> Option<&Type> {
        self.named_types.get(name)
    }

    pub fn declare_struct_type(&mut self, node: &'a Node<'a>) -> SemaResult<&Type> {
        let decl = match node.value {
            NodeValue::StructDecl(ref decl) => decl,
            _ => return sema_error("declare_struct_type() requires a StructDecl node".to_owned(), node),
        };

        let name = decl.name;

        if self.named_types.contains_key(name) {
            return sema_error(format!("redefinition of '{}'", name), node);
        }

        let struct_type = Type::StructType(
            StructType {
                name:   name,
                fields: try!(self.typecheck_struct_fields(decl)),
            }
        );

        Ok(self.named_types.entry(name).or_insert(struct_type))
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

            let field_type = try!(self.type_from_decl(&type_decl));

            // No pointers are allowed in a struct.
            // Arrays, optionals and uniques are checked for
            // explicitly and recursively, because they are
            // not like user-defined enums or structs in that
            // they might legitimately contain pointers when
            // contained within a class.
            try!(self.validate_field_type(field_type));

            if fields.insert(field.name, field_type).is_some() {
                return sema_error(format!("duplicate field '{}'", field.name), node);
            }
        }

        Ok(fields)
    }

    pub fn declare_enum_type(&mut self, decl: &EnumDecl) -> SemaResult<&EnumType> {
        unimplemented!()
    }

    pub fn declare_class_type(&mut self, decl: &ClassDecl) -> SemaResult<&ClassType> {
        unimplemented!()
    }

    fn type_from_decl(&mut self, decl: &Node) -> SemaResult<&'a Type<'a>> {
        unimplemented!()
    }

    fn validate_field_type(&mut self, field_type: &Type) -> SemaResult<()> {
        unimplemented!()
    }
}
