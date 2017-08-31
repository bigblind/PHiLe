//
// ast.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

//! This module contains types for building Abstract Syntax Trees
//! (ASTs). ASTs describe the syntactic structure of PHiLe programs.
//! Defined here are various kinds of AST nodes that the `parser`
//! module emits from a sequence of tokens produced by the `lexer`
//! module. AST nodes, like tokens, contain source location data
//! in order to easily map them to the original source code.

use lexer::{ Range, Ranged };


/// AST node representing an expression.
pub type Exp<'a>  = Node<ExpKind<'a>>;
/// AST node representing a type annotation.
pub type Ty<'a>   = Node<TyKind<'a>>;

/// Root of the AST.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Prog<'a> {
    /// The list of top-level items: type and function definitions or `impl`s.
    pub items: Vec<Item<'a>>,
}

/// Generic AST node (helper for Exp, Ty, etc.)
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Node<T> {
    /// Discriminant describing the type and value of the node.
    pub kind:  T,
    /// Source range that this node was generated from.
    pub range: Range,
}

/// A top-level source item.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Item<'a> {
    /// Definition of a `struct` type.
    StructDecl(StructDecl<'a>),
    /// Definition of a `class` type.
    ClassDecl(ClassDecl<'a>),
    /// Definition of an `enum` type.
    EnumDecl(EnumDecl<'a>),
    /// A global, free function.
    FuncDef(Function<'a>),
    /// `impl`, implementation of the methods of a type.
    Impl(Impl<'a>),
}

/// Type and value of an `Exp` expression node.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum ExpKind<'a> {
    /// `?:`, the Elvis operator.
    CondExp(Box<CondExp<'a>>),
    /// Any binary infix operator.
    BinaryOp(Box<BinaryOp<'a>>),
    /// A cast expression: `lhs as Rhs`
    Cast(Box<Exp<'a>>, Ty<'a>),
    /// Unary prefix plus operator.
    UnaryPlus(Box<Exp<'a>>),
    /// Unary prefix negation operator.
    UnaryMinus(Box<Exp<'a>>),
    /// Logical `not` operator.
    LogicNot(Box<Exp<'a>>),
    /// Indexing/subscripting postfix operator.
    Subscript(Box<Subscript<'a>>),
    /// Dot syntax: struct/class field or method access.
    MemberAccess(MemberAccess<'a>),
    /// Namespace access, `foo::bar`
    QualAccess(QualAccess<'a>),
    /// Function call
    FuncCall(FuncCall<'a>),
    /// Literal `nil`
    NilLiteral,
    /// Literal `true` or `false`.
    BoolLiteral(&'a str),
    /// An unparsed integer literal, without a sign.
    IntLiteral(&'a str),
    /// An unparsed floating-point literal.
    FloatLiteral(&'a str),
    /// A still potentially escaped string literal.
    StringLiteral(&'a str),
    /// The name of an entity, e.g. function, variable, or type.
    Identifier(&'a str),
    /// A tuple literal.
    TupleLiteral(Vec<Exp<'a>>),
    /// An array literal.
    ArrayLiteral(Vec<Exp<'a>>),
    /// A struct literal.
    StructLiteral(StructLiteral<'a>),
    /// An if expression or statement.
    If(Box<If<'a>>),
    /// A match expression or statement.
    Match(Match<'a>),
    /// A block expression or statement.
    Block(Vec<Exp<'a>>),
    /// A function literal, "lambda".
    FuncExp(Box<Function<'a>>),
    /// A local variable declaration.
    VarDecl(Box<VarDecl<'a>>),
    /// An expression turned into a statement by a trailing semicolon.
    Semi(Box<Exp<'a>>),
    /// An empty statement, just a semicolon.
    Empty,
}

/// Type and value of a `Ty` type node.
#[derive(Debug, PartialEq, Eq, Hash)]
pub enum TyKind<'a> {
    /// A pointer to the inner type.
    Pointer(Box<Ty<'a>>),
    /// An optional wrapping the inner type.
    Optional(Box<Ty<'a>>),
    /// A tuple of the inner types.
    Tuple(Vec<Ty<'a>>),
    /// An array of the inner type.
    Array(Box<Ty<'a>>),
    /// A function type.
    Function(FunctionTy<'a>),
    /// Named types: builtins and user-defined structs/classes/enums
    Named(&'a str),
}

//
// Top-level item helpers
//

/// Definition of a `struct` type.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct StructDecl<'a> {
    /// The source range of the type definition.
    pub range:  Range,
    /// The name of the struct type.
    pub name:   &'a str,
    /// The fields of the struct type.
    pub fields: Vec<Field<'a>>,
}

/// Definition of a `class` type.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ClassDecl<'a> {
    /// The source range of the type definition.
    pub range:  Range,
    /// The name of the class type.
    pub name:   &'a str,
    /// The fields of the class type.
    pub fields: Vec<Field<'a>>,
}

/// Declaration of an explicit relation between two class types.
/// A `RelDecl` node semantically means that the LHS of the
/// relation is the class in which the `RelDecl` is contained,
/// while the RHS is the type of the corresponding field.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct RelDecl<'a> {
    /// The cardinality operator.
    pub cardinality: &'a str,
    /// The name of the field in the RHS that points back to
    /// the LHS class, forming the inverse relation. If the
    /// relation has no explicit inverse, this is `None`.
    pub field:       Option<&'a str>,
}

/// A field specification within a struct or class type definition.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Field<'a> {
    /// The source range of the field definition.
    pub range:    Range,
    /// The name of the field.
    pub name:     &'a str,
    /// The declared type of the field.
    pub ty:       Ty<'a>,
    /// Optional explicit relation specification for the field.
    pub relation: Option<RelDecl<'a>>,
}

/// Definition of an `enum` type.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct EnumDecl<'a> {
    /// The source range of the type definition.
    pub range:    Range,
    /// The name of the `enum` type.
    pub name:     &'a str,
    /// The list of variants in the enum type.
    pub variants: Vec<Variant<'a>>,
}

/// Definition of one enum variant.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Variant<'a> {
    /// Source range of the definition.
    pub range: Range,
    /// The name of the variant.
    pub name:  &'a str,
    /// Type of the contained value of the variant, if any.
    pub ty:    Option<Ty<'a>>,
}

/// A function definition.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Function<'a> {
    /// Source range of the function.
    pub range:     Range,
    /// Name of the function if it is a named global, `None` for closures.
    pub name:      Option<&'a str>,
    /// The list of arguments to the function.
    pub arguments: Vec<FuncArg<'a>>,
    /// The return type specification, if present.
    pub ret_type:  Option<Ty<'a>>,
    /// The function body expression.
    pub body:      Exp<'a>,
}

/// Declaration of a single function argument.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FuncArg<'a> {
    /// Source range of the declaration.
    pub range: Range,
    /// Name of the argument.
    pub name:  &'a str,
    /// Type specification of the argument, if present.
    pub ty:    Option<Ty<'a>>,
}

/// Implementation of the methods of a user-defined type.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Impl<'a> {
    /// Source range if the `impl`.
    pub range:     Range,
    /// Name of the type for which methods are implemented.
    pub name:      &'a str,
    /// The list of methods in the implementation.
    pub functions: Vec<Function<'a>>,
}

//
// Expression kind helpers
//

/// A conditional expression or 'Elvis operator', `?:`.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct CondExp<'a> {
    /// The LHS is the condition.
    pub condition: Exp<'a>,
    /// The value when the condition is true. `None` when `?:` is
    /// used as the nil coalescing operator, e.g. `foo ?: default`.
    pub true_val:  Option<Exp<'a>>,
    /// The value when the condition is false or `nil`.
    pub false_val: Exp<'a>,
}

/// Any binary operator.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct BinaryOp<'a> {
    /// Textual representation of the operator itself.
    pub op:  &'a str,
    /// The left-hand side operand.
    pub lhs: Exp<'a>,
    /// The right-hand side operand.
    pub rhs: Exp<'a>,
}

/// An indexing or subscripting operator.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Subscript<'a> {
    /// The expression being indexed into.
    pub base:  Exp<'a>,
    /// The expression being used as the subscript in brackets.
    pub index: Exp<'a>,
}

/// Dot syntax for accessing fields and methods.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MemberAccess<'a> {
    /// The expression of which a field or method is requested.
    pub base:   Box<Exp<'a>>,
    /// The name of the field or method being accessed.
    pub member: &'a str,
}

/// Namespace-style member access, for e.g. enums.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct QualAccess<'a> {
    /// The expression of which a member is requested.
    pub base:   Box<Exp<'a>>,
    /// The name of the member.
    pub member: &'a str,
}

/// A function call operation.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FuncCall<'a> {
    /// The LHS, that is, the function being called.
    pub function:  Box<Exp<'a>>,
    /// The RHS, that is, the list of actual arguments.
    pub arguments: Vec<Exp<'a>>,
}

/// A struct literal.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct StructLiteral<'a> {
    /// Name of the struct type being instantiated.
    pub name:   &'a str,
    /// The list of fields: `(name, value)`.
    pub fields: Vec<(&'a str, Exp<'a>)>,
}

/// An if statement or expression.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct If<'a> {
    /// The Boolean condition of the `if`.
    pub condition: Exp<'a>,
    /// The expression evaulated when the condition is true.
    pub then_arm:  Exp<'a>,
    /// The expression evaluated when the condition is false.
    pub else_arm:  Option<Exp<'a>>,
}

/// A match statement or expression.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Match<'a> {
    /// The value to match.
    pub discriminant: Box<Exp<'a>>,
    /// The list of patterns and the expressions that are evaluated
    /// when one of them matches: `(pattern, expression)`.
    pub arms:         Vec<(Exp<'a>, Exp<'a>)>,
}

/// A variable declaration statement for locals.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct VarDecl<'a> {
    /// The name of the local variable.
    pub name: &'a str,
    /// The type specification of the variable, if present.
    pub ty:   Option<Ty<'a>>,
    /// The initializer expression of the variable.
    pub expr: Exp<'a>,
}

//
// Type kind helpers
//

/// A function type.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FunctionTy<'a> {
    /// The list of argument types the function takes, in order.
    pub arg_types: Vec<Ty<'a>>,
    /// The return type of the function.
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
