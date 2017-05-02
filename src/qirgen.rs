//
// qirgen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use qir::*;
use std::collections::HashMap;


#[allow(missing_debug_implementations)]
pub struct QIRGen<'a> {
    named_types: HashMap<&'a str, Type<'a>>,
    relations:   Vec<Relation<'a>>,
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

    // pub fn declare_struct_type(&mut self, decl: &StructDecl) -> Option<&StructType> {
    //     unimplemented!()
    // }

    // pub fn declare_enum_type(&mut self, decl: &EnumDecl) -> Option<&EnumType> {
    //     unimplemented!()
    // }

    // pub fn declare_class_type(&mut self, decl: &ClassDecl) -> Option<&ClassType> {
    //     unimplemented!()
    // }
}
