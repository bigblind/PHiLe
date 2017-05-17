//
// sqir.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::collections::HashMap;


// Types (part of the Schema)

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
    BlobType,
    DateType,

    OptionalType(&'a Type<'a>),
    UniqueType(&'a Type<'a>),
    PointerType(&'a Type<'a>),
    ArrayType(&'a Type<'a>),
    TupleType(Vec<&'a Type<'a>>),

    EnumType(EnumType<'a>),
    StructType(StructType<'a>),
    ClassType(ClassType<'a>),

    FunctionType(FunctionType<'a>),

    PlaceholderType(&'a str),
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

#[derive(Debug)]
pub struct FunctionType<'a> {
    pub arg_types: Vec<&'a Type<'a>>,
    pub ret_type:  &'a Type<'a>,
}

// Relations (also part of the Schema)

#[derive(Debug, Clone, Copy)]
pub struct RelationSide<'a> {
    pub class:       &'a Type<'a>,
    pub field:       Option<&'a str>,
    pub cardinality: Cardinality,
}

#[derive(Debug, Clone, Copy)]
pub enum Cardinality {
    ZeroOrOne,
    One,
    ZeroOrMore,
    OneOrMore,
}

pub type Relation<'a> = (RelationSide<'a>, RelationSide<'a>);

// Functions (Queries)

#[derive(Debug)]
pub struct Function<'a> {
    pub name:  &'a str,
    pub types: &'a Type<'a>, // wraps a FunctionType
    pub args:  Vec<&'a str>,
    // TODO(H2CO3): add body (instructions/basic blocks/etc.)
}

// Top-level type for wrapping SQIR for a complete program

#[derive(Debug)]
pub struct SQIR<'a> {
    pub named_types:    HashMap<&'a str, Type<'a>>, // builtins, structs, classes, enums, placeholders
    pub decimal_types:  HashMap<(usize, usize), Type<'a>>,
    pub optional_types: Vec<Type<'a>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub unique_types:   Vec<Type<'a>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub pointer_types:  Vec<Type<'a>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub array_types:    Vec<Type<'a>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub tuple_types:    Vec<Type<'a>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub function_types: Vec<Type<'a>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub relations:      Vec<Relation<'a>>,
    pub functions:      HashMap<&'a str, Function<'a>>,
}

impl<'a> SQIR<'a> {
    pub fn new() -> SQIR<'a> {
        let named_types = hash_map![
            "bool"   => Type::BoolType,
            "i8"     => Type::Int8Type,
            "u8"     => Type::UInt8Type,
            "i16"    => Type::Int16Type,
            "u16"    => Type::UInt16Type,
            "i32"    => Type::Int32Type,
            "u32"    => Type::UInt32Type,
            "i64"    => Type::Int64Type,
            "u64"    => Type::UInt64Type,
            "f32"    => Type::Float32Type,
            "f64"    => Type::Float64Type,
            "String" => Type::StringType,
            "Blob"   => Type::BlobType,
            "Date"   => Type::DateType,
        ];

        SQIR {
            named_types:    named_types,
            decimal_types:  hash_map![],
            optional_types: vec![],
            unique_types:   vec![],
            pointer_types:  vec![],
            array_types:    vec![],
            tuple_types:    vec![],
            function_types: vec![],
            relations:      vec![],
            functions:      hash_map![], // TODO(H2CO3): declare built-in functions
        }
    }
}
