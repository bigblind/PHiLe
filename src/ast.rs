//
// ast.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use lexer::Range;


#[derive(Debug)]
pub enum NodeValue<'a> {
    // Declarations / Definitions
    Program(Vec<Node<'a>>),
    Field(Box<Field<'a>>),
    StructDecl(StructDecl<'a>),
    ClassDecl(ClassDecl<'a>),
    Variant(Box<Variant<'a>>),
    EnumDecl(EnumDecl<'a>),
    FunctionDecl(FunctionDecl),

    // Types
    PointerType(Box<Node<'a>>),
    OptionalType(Box<Node<'a>>),
    UniqueType(Box<Node<'a>>),
    TupleType(Vec<Node<'a>>),
    ArrayType(Box<Node<'a>>),
    NamedType(&'a str),
}

#[derive(Debug)]
pub struct Node<'a> {
    pub range: Option<Range>,
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

#[derive(Debug)]
pub struct EnumDecl<'a> {
    pub name:     &'a str,
    pub variants: Vec<Node<'a>>,
}

#[derive(Debug)]
pub struct Variant<'a> {
    pub name:      &'a str,
    pub type_decl: Option<Node<'a>>,
}

#[allow(missing_copy_implementations)]
#[derive(Debug)]
pub struct FunctionDecl {
    // TODO(H2CO3): implement
}
