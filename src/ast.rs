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
    StructDecl(StructDecl<'a>),
    ClassDecl,
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
