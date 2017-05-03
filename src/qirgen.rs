//
// qirgen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::collections::HashMap;
use qir::*;
use lexer::*;
use ast;


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

    pub fn declare_struct_type(&mut self, decl: &ast::StructDecl) -> SemaResult<&StructType> {
        unimplemented!()
    }

    pub fn declare_enum_type(&mut self, decl: &ast::EnumDecl) -> SemaResult<&EnumType> {
        unimplemented!()
    }

    pub fn declare_class_type(&mut self, decl: &ast::ClassDecl) -> SemaResult<&ClassType> {
        unimplemented!()
    }
}
