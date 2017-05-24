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
    IntType,
    FloatType,
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

    PlaceholderType(String, PlaceholderKind),
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceholderKind {
    Struct,
    Class,
    Enum,
}

// A bit of terminology:
// * Complex types include enum, struct, class and tuple types.
// * Recursive but not complex types are pointers, optionals,
//   uniques and arrays.
// * Placeholders are temporaries that stand in for named types.
// * The rest of the types are called atomic or simple.
//   They include numeric types (bool, integral, floating-point),
//   Strings, Dates and Blobs.
// * User-defined types are enums, structs and classes.
// * Named types are also enums, structs and classes.
// * Product types are structs, classes and tuples.
// * Value types are enums, structs, and tuples.
// * Entity types are only classes (for now).
#[derive(Debug, Clone, Copy)]
pub enum ComplexTypeKind {
    Value,
    Entity,
}

#[derive(Debug)]
pub struct EnumType {
    pub name:     String,
    pub variants: HashMap<String, WkCell<Type>>,
    // Multiple types in a variant should be represented by a tuple
}

#[derive(Debug)]
pub struct StructType {
    pub name:   String,
    pub fields: HashMap<String, WkCell<Type>>,
}

#[derive(Debug)]
pub struct ClassType {
    pub name:   String,
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
    pub name:    String,
    pub fn_type: WkCell<Type>, // wraps a FunctionType
    pub args:    Vec<String>,
    // TODO(H2CO3): add body (instructions/basic blocks/etc.)
}

// Top-level type for wrapping SQIR for a complete program

#[derive(Debug)]
pub struct SQIR {
    // builtins, structs, classes, enums, placeholders
    pub named_types:    HashMap<String, RcCell<Type>>,
    pub decimal_types:  HashMap<(usize, usize), RcCell<Type>>,
    pub optional_types: HashMap<RcCell<Type>, RcCell<Type>>,
    pub unique_types:   HashMap<RcCell<Type>, RcCell<Type>>,
    pub pointer_types:  HashMap<RcCell<Type>, RcCell<Type>>,
    pub array_types:    HashMap<RcCell<Type>, RcCell<Type>>,
    pub tuple_types:    HashMap<Vec<RcCell<Type>>, RcCell<Type>>,
    pub function_types: HashMap<(Vec<RcCell<Type>>, RcCell<Type>), RcCell<Type>>,
    pub relations:      Vec<Relation>, // no need for a HashSet: relations are unique
    pub functions:      HashMap<String, Function>,
}

impl SQIR {
    pub fn new() -> SQIR {
        let named_types = hash_map![
            "bool"   => RcCell::new(Type::BoolType),
            "int"    => RcCell::new(Type::IntType),
            "float"  => RcCell::new(Type::FloatType),
            "String" => RcCell::new(Type::StringType),
            "Blob"   => RcCell::new(Type::BlobType),
            "Date"   => RcCell::new(Type::DateType),
        ];

        SQIR {
            named_types:    named_types,
            decimal_types:  hash_map![],
            optional_types: hash_map![],
            unique_types:   hash_map![],
            pointer_types:  hash_map![],
            array_types:    hash_map![],
            tuple_types:    hash_map![],
            function_types: hash_map![],
            relations:      vec![],
            functions:      hash_map![], // TODO(H2CO3): declare built-in functions
        }
    }
}
