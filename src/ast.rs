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
    Function(Box<Function<'a>>),
    FuncArg(FuncArg<'a>),
    Impl(Impl<'a>),

    // Statements
    VarDecl(Box<VarDecl<'a>>),
    EmptyStmt, // just a semicolon
    Semi(Box<Node<'a>>), // expression statement with trailing semicolon

    // Expressions, in ascending order of precedence
    CondExpr(Box<CondExpr<'a>>), // ?: Elvis operator
    BinaryOp(Box<BinaryOp<'a>>),

    UnaryPlus(Box<Node<'a>>),    // }
    UnaryMinus(Box<Node<'a>>),   // } Prefix ops
    LogicNot(Box<Node<'a>>),     // }

    Subscript(Box<Subscript<'a>>),  // }
    MemberAccess(MemberAccess<'a>), // } Postfix ops
    QualAccess(QualAccess<'a>),     // }
    FuncCall(FuncCall<'a>),         // }

    NilLiteral,
    BoolLiteral(bool),
    IntLiteral(u64),
    FloatLiteral(f64),
    StringLiteral(String), // unescaped
    Identifier(&'a str),

    TupleLiteral(Vec<Node<'a>>),
    ArrayLiteral(Vec<Node<'a>>),
    StructLiteral(StructLiteral<'a>),
    If(Box<If<'a>>),
    Match(Match<'a>),
    Block(Vec<Node<'a>>),

    // Types
    PointerType(Box<Node<'a>>),
    OptionalType(Box<Node<'a>>),
    UniqueType(Box<Node<'a>>),
    TupleType(Vec<Node<'a>>),
    ArrayType(Box<Node<'a>>),
    FunctionType(FunctionType<'a>),
    NamedType(&'a str),
}

#[derive(Debug)]
pub struct Node<'a> {
    pub range: Range,
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
pub struct Function<'a> {
    pub name:      Option<&'a str>,  // None iff closure
    pub arguments: Vec<Node<'a>>,    // FuncArg nodes
    pub ret_type:  Option<Node<'a>>, // type node
    pub body:      Node<'a>,         // expression node
}

#[derive(Debug)]
pub struct FuncArg<'a> {
    pub name:      &'a str,
    pub type_decl: Option<Box<Node<'a>>>, // type node
}

#[derive(Debug)]
pub struct Impl<'a> {
    pub name:      &'a str,
    pub functions: Vec<Node<'a>>, // Function nodes
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub name:      &'a str,
    pub type_decl: Option<Node<'a>>,
    pub init_expr: Option<Node<'a>>,
}

#[derive(Debug)]
pub struct CondExpr<'a> {
    pub condition: Node<'a>,
    pub true_val:  Node<'a>,
    pub false_val: Node<'a>,
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub op:  &'a str,
    pub lhs: Node<'a>,
    pub rhs: Node<'a>,
}

#[derive(Debug)]
pub struct Subscript<'a> {
    pub base:  Node<'a>,
    pub index: Node<'a>,
}

#[derive(Debug)]
pub struct MemberAccess<'a> {
    pub base:   Box<Node<'a>>,
    pub member: &'a str,
}

#[derive(Debug)]
pub struct QualAccess<'a> {
    pub base:   Box<Node<'a>>,
    pub member: &'a str,
}

#[derive(Debug)]
pub struct FuncCall<'a> {
    pub function:  Box<Node<'a>>,
    pub arguments: Vec<Node<'a>>,
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
    pub name:   &'a str,
    pub fields: Vec<(&'a str, Node<'a>)>,
}

#[derive(Debug)]
pub struct If<'a> {
    pub condition: Node<'a>,
    pub then_arm:  Node<'a>,
    pub else_arm:  Option<Node<'a>>,
}

#[derive(Debug)]
pub struct Match<'a> {
    pub discriminant: Box<Node<'a>>,
    pub arms:         Vec<Node<'a>>,
}

#[derive(Debug)]
pub struct FunctionType<'a> {
    pub arg_types: Vec<Node<'a>>,
    pub ret_type:  Box<Node<'a>>,
}
