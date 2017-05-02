//
// qir.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::collections::HashMap;


#[derive(Debug)]
pub enum Type<'a> {
    BoolType,

    Int8Type,
    UInt8Type,
    Int16Type,
    UInt16Type,
    Int32Type,
    UInt32Type,
    Int64Type,
    UInt64Type,

    Float32Type,
    Float64Type,

    DecimalType(usize, usize), // integral digits, fractional digits

    StringType,
    BinaryType,
    DateType,

    OptionalType(Box<Type<'a>>),
    UniqueType(Box<Type<'a>>),
    PointerType(Box<Type<'a>>),
    ArrayType(Box<Type<'a>>),
    TupleType(Vec<Type<'a>>),

    EnumType(EnumType<'a>),
    StructType(StructType<'a>),
    ClassType(ClassType<'a>),
}

#[derive(Debug)]
pub struct EnumType<'a> {
    pub name:     &'a str,
    pub variants: Vec<Variant<'a>>,
}

#[derive(Debug)]
pub struct Variant<'a> {
    pub name:  &'a str,
    pub types: Vec<&'a Type<'a>>,
}

#[derive(Debug)]
pub struct StructType<'a> {
    pub name:   &'a str,
    pub fields: HashMap<&'a str, &'a Type<'a>>,
}

#[derive(Debug)]
pub struct ClassType<'a> {
    pub name: &'a str,
    pub fields: HashMap<&'a str, &'a Type<'a>>,
}

#[derive(Debug, Clone, Copy)]
pub struct RelationSide<'a> {
    pub class:       &'a Type<'a>,
    pub field:       &'a str,
    pub cardinality: Cardinality,
}

#[derive(Debug, Clone, Copy)]
pub enum Cardinality {
    ZeroOrOne,
    One,
    ZeroPlus,
    OnePlus,
}

pub type Relation<'a> = (RelationSide<'a>, RelationSide<'a>);
