//
// sqir.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

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

#[derive(Debug)]
pub enum Type {
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

    Optional(WkType),
    Pointer(WkType),
    Array(WkType),
    Tuple(Vec<WkType>),

    Enum(EnumType),
    Struct(StructType),
    Class(ClassType),

    Function(FunctionType),

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
// * Complex types include enum, struct, class and tuple types.
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
#[derive(Debug, Clone, Copy)]
pub enum ComplexTypeKind {
    Value,
    Entity,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RelationSide {
    pub class:       RcType,
    pub field:       Option<String>,
    pub cardinality: Cardinality,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    // Function definition (lambda) + call.
    // Variable definitions (bindings) + references (substitutions)
    // are handled without explicit Expr nodes.
    Function(Function),
    Call(Call),

    // Type conversions: implicit T -> T?, Int -> Float,
    // and explicit T -> unit (Semi).
    // Implicit conversions are inserted by the unify()
    // function, which always creates a new expression
    // for each encountered implicit conversion. Therefore
    // these conversions cannot form a reference cycle,
    // so it's OK to make their argument strong, so that
    // they don't have to be inserted into `temporaries`.
    OptionalWrap(RcExpr),
    IntToFloat(RcExpr),
    Ignore(WkExpr),

    // Arithmetic, comparison, logical and set operations
    Neg(WkExpr),
    Add(WkExpr, WkExpr),
    Sub(WkExpr, WkExpr),
    Mul(WkExpr, WkExpr),
    Div(WkExpr, WkExpr),
    Mod(WkExpr, WkExpr),

    Eq(WkExpr, WkExpr),
    Neq(WkExpr, WkExpr),
    Lt(WkExpr, WkExpr),
    LtEq(WkExpr, WkExpr),
    Gt(WkExpr, WkExpr),
    GtEq(WkExpr, WkExpr),

    And(WkExpr, WkExpr),
    Or(WkExpr, WkExpr),
    Not(WkExpr),

    Intersect(WkExpr, WkExpr),
    Union(WkExpr, WkExpr),
    SetDiff(WkExpr, WkExpr),

    // Branch
    // TODO(H2CO3): generalize for match & arbitrary patterns
    Branch(Branch),

    // Blocks and aggreate literals
    Seq(Vec<WkExpr>),
    ArrayLiteral(Vec<WkExpr>),
    TupleLiteral(Vec<WkExpr>),
    StructLiteral(BTreeMap<String, WkExpr>),

    // Generalized Built-in DB operations
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

#[derive(Debug, Clone)]
pub struct Function {
    pub arg_names: Vec<String>, // TODO(H2CO3): maybe this should be a Vec<RcExpr> holding arguments?
    pub body:      WkExpr,
}

#[derive(Debug)]
pub struct Call {
    pub callee: WkExpr,
    pub args:   Vec<WkExpr>,
}

#[derive(Debug)]
pub struct Branch {
    pub discriminant: WkExpr,
    pub arms:         Vec<(WkExpr, WkExpr)>,
}

#[derive(Debug)]
pub struct Map {
    pub range: WkExpr,
    pub op:    WkExpr,
}

#[derive(Debug)]
pub struct Reduce {
    pub range: WkExpr,
    pub zero:  WkExpr,
    pub op:    WkExpr,
}

#[derive(Debug)]
pub struct Filter {
    pub range: WkExpr,
    pub pred:  WkExpr,
}

#[derive(Debug)]
pub struct Sort {
    pub range: WkExpr,
    pub cmp:   WkExpr,
}

#[derive(Debug)]
pub struct Group {
    pub range: WkExpr,
    pub pred:  WkExpr,
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
    pub temporaries:    Vec<RcExpr>,
}


// This is to be used ONLY when you know you have a Class type
pub fn unwrap_class_name(class: &RcType) -> String {
    match *class.borrow().expect("Cannot borrow Type::Class?!") {
        Type::Class(ref c) => c.name.clone(),
        _ => unreachable!("Non-class class type?!"),
    }
}

// `RelationSide: PartialOrd + Ord` is necessary for
// order-independent hashing of `Relation`s.
impl PartialOrd for RelationSide {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

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
        self.lhs == other.lhs && self.rhs == other.rhs
        ||
        self.lhs == other.rhs && self.rhs == other.lhs
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
        // TODO(H2CO3): match these with get_bool_type(), etc. in sqirgen.rs
        let named_types = btree_map![
            "bool"   => RcCell::new(Type::Bool),
            "int"    => RcCell::new(Type::Int),
            "float"  => RcCell::new(Type::Float),
            "String" => RcCell::new(Type::String),
            "Blob"   => RcCell::new(Type::Blob),
            "Date"   => RcCell::new(Type::Date),
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
            temporaries:    Vec::new(),
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
