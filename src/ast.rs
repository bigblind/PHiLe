//
// ast.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use lexer::Token;


#[derive(Debug)]
pub enum NodeValue<'a> {
    Program(Vec<Node<'a>>),
    Field(Box<Field<'a>>),
    StructDecl(StructDecl<'a>),
    ClassDecl(ClassDecl<'a>),
    EnumDecl,
    FunctionDecl,
}

#[derive(Debug)]
pub struct Node<'a> {
    pub begin: Option<&'a Token<'a>>,
    pub end:   Option<&'a Token<'a>>,
    pub value: NodeValue<'a>,
}

#[derive(Debug)]
pub struct StructDecl<'a> {
    pub name:   &'a str,
    pub fields: Vec<Node<'a>>,
}

#[derive(Debug)]
pub struct ClassDecl<'a> {
    pub name:       &'a str,
    pub superclass: Option<&'a str>,
    pub fields:     Vec<Node<'a>>,
}

#[derive(Debug)]
pub struct Relation<'a> {
    pub cardinality: &'a str,
    pub entity:      &'a str,
    pub field:       &'a str,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub name:      &'a str,
    pub type_decl: Option<Node<'a>>,
    pub relation:  Option<Relation<'a>>,
}
