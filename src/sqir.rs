//
// sqir.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::collections::HashMap;
use util::*;


// Types (part of the Schema)

#[derive(Debug)]
pub enum Type {
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

    OptionalType(WkCell<Type>),
    UniqueType(WkCell<Type>),
    PointerType(WkCell<Type>),
    ArrayType(WkCell<Type>),
    TupleType(Vec<WkCell<Type>>),

    EnumType(EnumType),
    StructType(StructType),
    ClassType(ClassType),

    FunctionType(FunctionType),

    PlaceholderType(String),
}

#[derive(Debug)]
pub struct EnumType {
    pub name:     String,
    pub variants: Vec<Variant>,
}

#[derive(Debug)]
pub struct Variant {
    pub name:  String,
    pub types: Vec<WkCell<Type>>,
}

#[derive(Debug)]
pub struct StructType {
    pub name:   String,
    pub fields: HashMap<String, WkCell<Type>>,
}

#[derive(Debug)]
pub struct ClassType {
    pub name: String,
    pub fields: HashMap<String, WkCell<Type>>,
}

#[derive(Debug)]
pub struct FunctionType {
    pub arg_types: Vec<WkCell<Type>>,
    pub ret_type:  WkCell<Type>,
}

// Relations (also part of the Schema)

#[derive(Debug)]
pub struct RelationSide {
    pub class:       WkCell<Type>,
    pub field:       Option<String>,
    pub cardinality: Cardinality,
}

#[derive(Debug, Clone, Copy)]
pub enum Cardinality {
    ZeroOrOne,
    One,
    ZeroOrMore,
    OneOrMore,
}

pub type Relation = (RelationSide, RelationSide);

// Functions (Queries)

#[derive(Debug)]
pub struct Function {
    pub name:  String,
    pub types: WkCell<Type>, // wraps a FunctionType
    pub args:  Vec<String>,
    // TODO(H2CO3): add body (instructions/basic blocks/etc.)
}

// Top-level type for wrapping SQIR for a complete program

#[derive(Debug)]
pub struct SQIR {
    pub named_types:    HashMap<String, RcCell<Type>>, // builtins, structs, classes, enums, placeholders
    pub decimal_types:  HashMap<(usize, usize), RcCell<Type>>,
    pub optional_types: Vec<RcCell<Type>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub unique_types:   Vec<RcCell<Type>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub pointer_types:  Vec<RcCell<Type>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub array_types:    Vec<RcCell<Type>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub tuple_types:    Vec<RcCell<Type>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub function_types: Vec<RcCell<Type>>, // TODO(H2CO3): replace with HashSet for O(1)
    pub relations:      Vec<Relation>,
    pub functions:      HashMap<String, Function>,
}

impl SQIR {
    pub fn new() -> SQIR {
        let named_types = hash_map![
            "bool"   => RcCell::new(Type::BoolType),
            "i8"     => RcCell::new(Type::Int8Type),
            "u8"     => RcCell::new(Type::UInt8Type),
            "i16"    => RcCell::new(Type::Int16Type),
            "u16"    => RcCell::new(Type::UInt16Type),
            "i32"    => RcCell::new(Type::Int32Type),
            "u32"    => RcCell::new(Type::UInt32Type),
            "i64"    => RcCell::new(Type::Int64Type),
            "u64"    => RcCell::new(Type::UInt64Type),
            "f32"    => RcCell::new(Type::Float32Type),
            "f64"    => RcCell::new(Type::Float64Type),
            "String" => RcCell::new(Type::StringType),
            "Blob"   => RcCell::new(Type::BlobType),
            "Date"   => RcCell::new(Type::DateType),
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
