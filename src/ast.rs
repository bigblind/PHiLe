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
    FuncDecl(FuncDecl<'a>),
    Impl(Impl<'a>),
    VarDecl(VarDecl<'a>),

    // Types
    PointerType(Box<Node<'a>>),
    OptionalType(Box<Node<'a>>),
    UniqueType(Box<Node<'a>>),
    TupleType(Vec<Node<'a>>),
    ArrayType(Box<Node<'a>>),
    NamedType(&'a str),

    // Expressions
    Block(Vec<Node<'a>>),
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
pub struct RelDecl<'a> {
    pub cardinality: &'a str,
    pub field:       Option<&'a str>,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub name:      &'a str,
    pub type_decl: Node<'a>,
    pub relation:  Option<RelDecl<'a>>,
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

#[derive(Debug)]
pub struct FuncDecl<'a> {
    pub name:        &'a str,
    pub return_type: Option<Box<Node<'a>>>, // type node
    pub arguments:   Vec<Node<'a>>,         // VarDecl nodes
    pub body:        Box<Node<'a>>,         // Block node
}

#[derive(Debug)]
pub struct Impl<'a> {
    pub name:      &'a str,
    pub functions: Vec<Node<'a>>, // FunctionDecl nodes
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub name:      &'a str,
    pub type_decl: Option<Box<Node<'a>>>,
    pub init_expr: Option<Box<Node<'a>>>,
}
