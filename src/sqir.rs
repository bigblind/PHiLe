//
// sqir.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::fmt::{ self, Debug, Display, Formatter };
use std::collections::{ HashMap, BTreeMap, BTreeSet };
use std::cmp::*;
use util::*;


pub type RcType = RcCell<Type>;
pub type WkType = WkCell<Type>;
pub type RcExpr = RcCell<Expr>;
pub type WkExpr = WkCell<Expr>;

//
// Types (part of the Schema)
//

#[derive(Debug, Clone, Copy)]
pub struct BuiltinName {
    pub bool_name:    &'static str,
    pub int_name:     &'static str,
    pub float_name:   &'static str,
    pub decimal_name: &'static str,
    pub string_name:  &'static str,
    pub blob_name:    &'static str,
    pub date_name:    &'static str,
}

pub static BUILTIN_NAME: BuiltinName = BuiltinName {
    bool_name:    "bool",
    int_name:     "int",
    float_name:   "float",
    decimal_name: "Decimal",
    string_name:  "String",
    blob_name:    "Blob",
    date_name:    "Date",
};

#[derive(Debug)]
pub enum Type {
    // Numeric and Complex Built-in Types
    Bool,
    Int,
    Float,
    Decimal {
        integral:   usize,
        fractional: usize,
    },

    String,
    Blob,
    Date,

    // Container/Wrapper Types
    Optional(WkType),
    Pointer(WkType),
    Array(WkType),
    Tuple(Vec<WkType>),

    // User-defined Types
    Enum(EnumType),
    Struct(StructType),
    Class(ClassType),

    // Function Type
    Function(FunctionType),

    // Placeholder Type (only in type graph during typechecking)
    Placeholder {
        name: String,
        kind: PlaceholderKind,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum PlaceholderKind {
    Struct,
    Class,
    Enum,
}

// A bit of terminology:
// * Complex types include enum, struct, class, tuple and function types.
// * Recursive but not complex types are pointers, optionals,
//   and arrays.
// * Placeholders are temporaries that stand in for named types.
// * The rest of the types are called atomic or simple.
//   They include numeric types (bool, integral, floating-point),
//   Strings, Dates and Blobs.
// * User-defined types are enums, structs and classes.
// * Named types are also enums, structs and classes.
// * Product types are structs, classes and tuples.
// * Value types are enums, structs, and tuples.
// * Entity types are only classes (for now).
// * Function types are... function types, obviously.
#[derive(Debug, Clone, Copy)]
pub enum ComplexTypeKind {
    Value,
    Entity,
    Function,
}

#[derive(Debug)]
pub struct EnumType {
    pub name:     String,
    pub variants: BTreeMap<String, WkType>,
}

#[derive(Debug)]
pub struct StructType {
    pub name:   String,
    pub fields: BTreeMap<String, WkType>,
}

#[derive(Debug)]
pub struct ClassType {
    pub name:   String,
    pub fields: BTreeMap<String, WkType>,
}

#[derive(Debug, Clone)]
pub struct FunctionType {
    pub arg_types: Vec<WkType>,
    pub ret_type:  WkType,
}

//
// Relations (also part of the Schema)
//

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelationSide {
    pub class:       RcType,
    pub field:       Option<String>,
    pub cardinality: Cardinality,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Cardinality {
    ZeroOrOne,
    One,
    ZeroOrMore,
    OneOrMore,
}

#[derive(Debug)]
pub struct Relation {
    pub lhs: RelationSide,
    pub rhs: RelationSide,
}

//
// Functions (Queries)
//

// Lambda calculus node
#[derive(Debug)]
pub struct Expr {
    pub ty:    RcType, // can be Rc: no cycles are possible
    pub value: Value,
    pub id:    ExprId,
}

#[derive(Debug)]
pub enum Value {
    ////////////////////////////////////////////////////////////
    // Core -- only the variants below are emitted by SQIRGen //
    ////////////////////////////////////////////////////////////

    // Value of a forward-declared function
    Placeholder,

    // Constants (mostly generated from literals)
    Nil,
    BoolConst(bool),
    IntConst(u64),
    FloatConst(f64),
    StringConst(String),

    // Lambda calculus backbone:
    // Function definition (lambda) + call;
    // Function argument + name reference (substitutions).
    // Variable declarations are implicit using named expressions.
    // A strong pointer inside a VarDecl is OK, since the
    // initializer expression cannot refer back to the variable
    // FuncArg::index is to be interpreted within the function,
    // NOT within the context of all currently-visible locals.
    // The wrapped expression of Load is the expression referred
    // to by the name -- either a global (of any kind), or the
    // init expression of a local VarDecl (might be a FuncArg).
    // The loaded expression must be weak, because the body of a
    // named (eg. global) recursive function can refer to itself.
    Function(Function),
    Call(Call),
    FuncArg { func: WkExpr, index: usize },
    Load(WkExpr),

    // Type conversions: implicit T -> T?,
    // and explicit T -> unit (Semi).
    // Implicit conversions are inserted by the unify()
    // function, which always creates a new expression
    // for each encountered implicit conversion. Therefore
    // these conversions cannot form a reference cycle,
    // so it's OK to make their argument strong, so that
    // they don't have to be inserted into `temporaries`.
    OptionalWrap(RcExpr),
    Ignore(RcExpr),

    // Arithmetic, comparison, logical and set operations
    Neg(RcExpr),
    Add(RcExpr, RcExpr),
    Sub(RcExpr, RcExpr),
    Mul(RcExpr, RcExpr),
    Div(RcExpr, RcExpr),
    Mod(RcExpr, RcExpr),

    Eq(RcExpr, RcExpr),
    Neq(RcExpr, RcExpr),
    Lt(RcExpr, RcExpr),
    LtEq(RcExpr, RcExpr),
    Gt(RcExpr, RcExpr),
    GtEq(RcExpr, RcExpr),

    And(RcExpr, RcExpr),
    Or(RcExpr, RcExpr),
    Not(RcExpr),

    Intersect(RcExpr, RcExpr),
    Union(RcExpr, RcExpr),
    SetDiff(RcExpr, RcExpr),

    // Branch
    Branch(Branch),

    // Blocks and aggreate literals
    Seq(Vec<RcExpr>),
    Array(Vec<RcExpr>),
    Tuple(Vec<RcExpr>),
    StructLiteral(BTreeMap<String, RcExpr>),

    // Generalized built-in DB operations
    Map(Map),       // projections etc.
    Reduce(Reduce), // aggregations
    Filter(Filter), // selection
    Sort(Sort),
    Group(Group),
    Join,   // TODO(H2CO3): design + implement
    Insert, // TODO(H2CO3): design + implement
    Update, // TODO(H2CO3): design + implement

    ///////////////////////////////////////////////////
    // Common DB operations, only emitted by SQIROpt //
    ///////////////////////////////////////////////////

    // TODO(H2CO3): _actually_ design these

    // Simple projection, sorting and grouping based on columns
    Project(RcType, Vec<String>),
    GroupBy(RcType, Vec<String>),
    SortBy(RcType, Vec<String>),

    // Common aggregations on a single column / independent columns
    Sum(RcType, String),
    Avg(RcType, String),
    Min(RcType, String),
    Max(RcType, String),
}

#[derive(Debug)]
pub enum ExprId {
    Temp(usize),
    Local(String),
    Global(String),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub args: Vec<RcExpr>, // FuncArg expressions
    pub body: RcExpr,
}

#[derive(Debug)]
pub struct Call {
    pub callee: RcExpr,
    pub args:   Vec<RcExpr>,
}

#[derive(Debug)]
pub struct Branch {
    pub discriminant: RcExpr,
    pub arms:         Vec<(RcExpr, RcExpr)>,
}

#[derive(Debug)]
pub struct Map {
    pub range: RcExpr,
    pub op:    RcExpr,
}

#[derive(Debug)]
pub struct Reduce {
    pub range: RcExpr,
    pub zero:  RcExpr,
    pub op:    RcExpr,
}

#[derive(Debug)]
pub struct Filter {
    pub range: RcExpr,
    pub pred:  RcExpr,
}

#[derive(Debug)]
pub struct Sort {
    pub range: RcExpr,
    pub cmp:   RcExpr,
}

#[derive(Debug)]
pub struct Group {
    pub range: RcExpr,
    pub pred:  RcExpr,
}

//
// Top-level type for wrapping SQIR for a complete program
//

#[derive(Debug)]
pub struct SQIR {
    pub named_types:    BTreeMap<String, RcType>,
    pub decimal_types:  HashMap<(usize, usize), RcType>,
    pub optional_types: HashMap<RcType, RcType>,
    pub pointer_types:  HashMap<RcType, RcType>,
    pub array_types:    HashMap<RcType, RcType>,
    pub tuple_types:    HashMap<Vec<RcType>, RcType>,
    pub function_types: HashMap<(Vec<RcType>, RcType), RcType>,
    pub relations:      HashMap<(RcType, String), Relation>,
    pub globals:        BTreeMap<Option<String>, BTreeMap<String, RcExpr>>,
}


// This is to be used ONLY when you know you have a Class type
pub fn unwrap_class_name(class: &RcType) -> String {
    match *class.borrow().expect("Cannot borrow Type::Class?!") {
        Type::Class(ref c) => c.name.clone(),
        _ => unreachable!("Non-class class type?!"),
    }
}

fn write_many_types(f: &mut Formatter, types: &[WkType]) -> fmt::Result {
    f.write_str("(")?;

    for (index, ty) in types.iter().enumerate() {
        let sep = if index > 0 { ", " } else { "" };
        write!(f, "{}{}", sep, WeakDisplay(ty))?
    }

    f.write_str(")")
}

fn write_decimal_type(f: &mut Formatter, integral: usize, fractional: usize) -> fmt::Result {
    write!(
        f,
        "{}<{}.{}>",
        BUILTIN_NAME.decimal_name,
        integral,
        fractional
    )
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match *self {
            Type::Bool   => f.write_str(BUILTIN_NAME.bool_name),
            Type::Int    => f.write_str(BUILTIN_NAME.int_name),
            Type::Float  => f.write_str(BUILTIN_NAME.float_name),
            Type::String => f.write_str(BUILTIN_NAME.string_name),
            Type::Blob   => f.write_str(BUILTIN_NAME.blob_name),
            Type::Date   => f.write_str(BUILTIN_NAME.date_name),

            Type::Decimal { integral, fractional } => {
                write_decimal_type(f, integral, fractional)
            },
            Type::Optional(ref wrapped) => {
                write!(f, "({})?", WeakDisplay(wrapped))
            },
            Type::Pointer(ref pointed) => {
                write!(f, "&({})", WeakDisplay(pointed))
            },
            Type::Array(ref element) => {
                write!(f, "[{}]", WeakDisplay(element))
            },
            Type::Function(ref ty) => {
                write_many_types(f, &ty.arg_types)?;
                write!(f, " -> {}", WeakDisplay(&ty.ret_type))
            },
            Type::Placeholder { ref name, kind } => {
                write!(f, "Placeholder({}, kind: {})", name, kind)
            },

            Type::Tuple(ref types) => write_many_types(f, types),
            Type::Enum(ref e)   => f.write_str(&e.name),
            Type::Struct(ref s) => f.write_str(&s.name),
            Type::Class(ref c)  => f.write_str(&c.name),
        }
    }
}

impl_display_as_debug! {
    PlaceholderKind,
    Cardinality,
}

impl PartialOrd for RelationSide {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// TODO(H2CO3): 2 calls to unwrap_class_name() means 2 allocations
// per comparison. This is grossly wasteful and should be improved.
impl Ord for RelationSide {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_name  = unwrap_class_name(&self.class);
        let other_name = unwrap_class_name(&other.class);
        let lhs = (self_name,  &self.field,  self.cardinality);
        let rhs = (other_name, &other.field, other.cardinality);

        lhs.cmp(&rhs)
    }
}

impl PartialEq for Relation {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Relation {}

impl PartialOrd for Relation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Relation {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_min  = min(&self.lhs,  &self.rhs);
        let self_max  = max(&self.lhs,  &self.rhs);
        let other_min = min(&other.lhs, &other.rhs);
        let other_max = max(&other.lhs, &other.rhs);
        Ord::cmp(&(self_min, self_max), &(other_min, other_max))
    }
}

impl SQIR {
    pub fn new() -> SQIR {
        let named_types = btree_map![
            BUILTIN_NAME.bool_name   => RcCell::new(Type::Bool),
            BUILTIN_NAME.int_name    => RcCell::new(Type::Int),
            BUILTIN_NAME.float_name  => RcCell::new(Type::Float),
            BUILTIN_NAME.string_name => RcCell::new(Type::String),
            BUILTIN_NAME.blob_name   => RcCell::new(Type::Blob),
            BUILTIN_NAME.date_name   => RcCell::new(Type::Date),
        ];

        SQIR {
            named_types:    named_types,
            decimal_types:  HashMap::new(),
            optional_types: HashMap::new(),
            pointer_types:  HashMap::new(),
            array_types:    HashMap::new(),
            tuple_types:    HashMap::new(),
            function_types: HashMap::new(),
            relations:      HashMap::new(),
            globals:        BTreeMap::new(), // TODO(H2CO3): declare built-in functions
        }
    }

    // Relations can be stored either once or twice in the
    // `relations` field (because they need to be associated
    // with named sides for easy querygen), but schemagen
    // still requireds a non-redundant set of relations.
    // By convention, `relations` contains relations in a
    // way that the RelationSide corresponding to the key
    // appears on the LHS, whereas the referred RelationSide
    // is the RHS.
    pub fn unique_relations(&self) -> BTreeSet<&Relation> {
        self.relations.values().collect()
    }
}
