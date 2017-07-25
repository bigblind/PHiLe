//
// ast.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use lexer::{ Range, Ranged };


pub type Prog<'a> = Vec<Item<'a>>;
pub type Exp<'a>  = Node<ExpKind<'a>>;
pub type Ty<'a>   = Node<TyKind<'a>>;

#[derive(Debug)]
pub struct Node<T> {
    pub kind:  T,
    pub range: Range,
}

#[derive(Debug)]
pub enum Item<'a> {
    StructDecl(StructDecl<'a>),
    ClassDecl(ClassDecl<'a>),
    EnumDecl(EnumDecl<'a>),
    FuncDef(Function<'a>),
    Impl(Impl<'a>),
}

#[derive(Debug)]
pub enum ExpKind<'a> {
    CondExp(Box<CondExp<'a>>), // ?: Elvis operator
    BinaryOp(Box<BinaryOp<'a>>),

    UnaryPlus(Box<Exp<'a>>),    // }
    UnaryMinus(Box<Exp<'a>>),   // } Prefix ops
    LogicNot(Box<Exp<'a>>),     // }

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

    TupleLiteral(Vec<Exp<'a>>),
    ArrayLiteral(Vec<Exp<'a>>),
    StructLiteral(StructLiteral<'a>),
    If(Box<If<'a>>),
    Match(Match<'a>),
    Block(Vec<Exp<'a>>),
    FuncExp(Box<Function<'a>>),

    // Statement expressions
    VarDecl(Box<VarDecl<'a>>), // local variable declaration
    Semi(Box<Exp<'a>>),        // expression with trailing semicolon
    Empty,                     // just a semicolon
}

#[derive(Debug)]
pub enum TyKind<'a> {
    Pointer(Box<Ty<'a>>),
    Optional(Box<Ty<'a>>),
    Tuple(Vec<Ty<'a>>),
    Array(Box<Ty<'a>>),
    Function(FunctionTy<'a>),
    Named(&'a str),
}

//
// Top-level item helpers
//

#[derive(Debug)]
pub struct StructDecl<'a> {
    pub range:  Range,
    pub name:   &'a str,
    pub fields: Vec<Field<'a>>,
}

#[derive(Debug)]
pub struct ClassDecl<'a> {
    pub range:  Range,
    pub name:   &'a str,
    pub fields: Vec<Field<'a>>,
}

#[derive(Debug)]
pub struct RelDecl<'a> {
    pub cardinality: &'a str,
    pub field:       Option<&'a str>,
}

#[derive(Debug)]
pub struct Field<'a> {
    pub range:    Range,
    pub name:     &'a str,
    pub ty:       Ty<'a>,
    pub relation: Option<RelDecl<'a>>,
}

#[derive(Debug)]
pub struct EnumDecl<'a> {
    pub range:    Range,
    pub name:     &'a str,
    pub variants: Vec<Variant<'a>>,
}

#[derive(Debug)]
pub struct Variant<'a> {
    pub range: Range,
    pub name:  &'a str,
    pub ty:    Option<Ty<'a>>,
}

#[derive(Debug)]
pub struct Function<'a> {
    pub range:     Range,
    pub name:      Option<&'a str>,  // None iff closure
    pub arguments: Vec<FuncArg<'a>>,
    pub ret_type:  Option<Ty<'a>>,
    pub body:      Exp<'a>,
}

#[derive(Debug)]
pub struct FuncArg<'a> {
    pub range: Range,
    pub name:  &'a str,
    pub ty:    Option<Ty<'a>>,
}

#[derive(Debug)]
pub struct Impl<'a> {
    pub range:     Range,
    pub name:      &'a str,
    pub functions: Vec<Function<'a>>,
}

//
// Expression kind helpers
//

#[derive(Debug)]
pub struct CondExp<'a> {
    pub condition: Exp<'a>,
    pub true_val:  Exp<'a>,
    pub false_val: Exp<'a>,
}

#[derive(Debug)]
pub struct BinaryOp<'a> {
    pub op:  &'a str,
    pub lhs: Exp<'a>,
    pub rhs: Exp<'a>,
}

#[derive(Debug)]
pub struct Subscript<'a> {
    pub base:  Exp<'a>,
    pub index: Exp<'a>,
}

#[derive(Debug)]
pub struct MemberAccess<'a> {
    pub base:   Box<Exp<'a>>,
    pub member: &'a str,
}

#[derive(Debug)]
pub struct QualAccess<'a> {
    pub base:   Box<Exp<'a>>,
    pub member: &'a str,
}

#[derive(Debug)]
pub struct FuncCall<'a> {
    pub function:  Box<Exp<'a>>,
    pub arguments: Vec<Exp<'a>>,
}

#[derive(Debug)]
pub struct StructLiteral<'a> {
    pub name:   &'a str,
    pub fields: Vec<(&'a str, Exp<'a>)>,
}

#[derive(Debug)]
pub struct If<'a> {
    pub condition: Exp<'a>,
    pub then_arm:  Exp<'a>,
    pub else_arm:  Option<Exp<'a>>,
}

#[derive(Debug)]
pub struct Match<'a> {
    pub discriminant: Box<Exp<'a>>,
    pub arms:         Vec<(Exp<'a>, Exp<'a>)>,
}

#[derive(Debug)]
pub struct VarDecl<'a> {
    pub name: &'a str,
    pub ty:   Option<Ty<'a>>,
    pub expr: Exp<'a>,
}

//
// Type kind helpers
//

#[derive(Debug)]
pub struct FunctionTy<'a> {
    pub arg_types: Vec<Ty<'a>>,
    pub ret_type:  Box<Ty<'a>>,
}

//
// Impl Ranged for pretty much everything
//

macro_rules! impl_ranged {
    ($($name: ident),*) => ($(
        impl<'a> Ranged for $name<'a> {
            fn range(&self) -> Range {
                self.range
            }
        }
    )*);
    ($($name: ident),+,) => { impl_ranged!($($name),+); };
}

impl_ranged! {
    StructDecl,
    ClassDecl,
    Field,
    EnumDecl,
    Variant,
    Function,
    FuncArg,
    Impl,
}

impl<T> Ranged for Node<T> {
    fn range(&self) -> Range {
        self.range
    }
}

impl<'a> Ranged for Item<'a> {
    fn range(&self) -> Range {
        match *self {
            Item::StructDecl(ref s) => s.range,
            Item::ClassDecl(ref c)  => c.range,
            Item::EnumDecl(ref e)   => e.range,
            Item::FuncDef(ref f)    => f.range,
            Item::Impl(ref i)       => i.range,
        }
    }
}
