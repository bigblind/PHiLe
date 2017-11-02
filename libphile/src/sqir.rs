//
// sqir.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

//! Defines SQIR, the Schema and Query Intermediate Representation.
//! SQIR is the intermediate language of PHiLe, on which optimizations
//! are performed. SQIR is also the starting point of code generation
//! for database abstraction layers.

use std::fmt::{ self, Display, Formatter };
use std::collections::{ HashMap, BTreeMap, BTreeSet };
use std::cmp::*;
use util::*;


/// Convenience type alias for a reference-counted type descriptor.
pub type RcType = RcCell<Type>;
/// Weak counterpart of `RcType`.
pub type WkType = WkCell<Type>;
/// Convenience type alias for a reference-counted expression node.
pub type RcExpr = RcCell<Expr>;
/// Weak counterpart of `RcExpr`.
pub type WkExpr = WkCell<Expr>;

//
// Types (part of the Schema)
//

/// Centralized container for the name of each builtin type.
#[allow(missing_docs)] // field names are self-explanatory...
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

/// The "singleton" `BuiltinName` instance.
pub static BUILTIN_NAME: BuiltinName = BuiltinName {
    bool_name:    "bool",
    int_name:     "int",
    float_name:   "float",
    decimal_name: "Decimal",
    string_name:  "String",
    blob_name:    "Blob",
    date_name:    "Date",
};

/// `Type` is deliberately not `Clone`, `PartialEq` and `Eq`:
/// it is meant to be used only with `RcCell`/`WkCell`,
/// and its instances should be compared by address.
#[derive(Debug)]
pub enum Type {
    /// Boolean type.
    Bool,
    /// Signed integer type.
    Int,
    /// Floating-point type.
    Float,
    /// Decimal number type.
    Decimal {
        /// The number of digits in the integral part of the number.
        integral:   usize,
        /// The number of digits in the fractional part of the number.
        fractional: usize,
    },

    /// String type.
    String,
    /// Binary Large Object type.
    Blob,
    /// Date type; a single point in time.
    Date,

    /// Optional type: either a wrapped value or `nil`.
    Optional(WkType),
    /// Pointer type. An external reference to an entity.
    Pointer(WkType),
    /// Array type. An ordered list of values.
    Array(WkType),
    /// Tuple type. An ad-hoc product type.
    Tuple(Vec<WkType>),

    /// An enumerated (sum) type.
    Enum(EnumType),
    /// A structure type: a product type with named fields.
    Struct(StructType),
    /// A class type: a product and entity type with named fields.
    Class(ClassType),

    /// A Function type.
    Function(FunctionType),

    /// A placeholder type. Only exists during typechecking.
    Placeholder {
        /// The name of the type this placeholder temporarily stands for.
        name: String,
        /// The kind of the type this placeholder stands for.
        kind: PlaceholderKind,
    },
}

/// The kind of the type a placeholder type stands for.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PlaceholderKind {
    /// Indicates that the placeholder stands for a `struct` type.
    Struct,
    /// Indicates that the placeholder stands for a `class` type.
    Class,
    /// Indicates that the placeholder stands for an `enum` type.
    Enum,
}

/// A bit of terminology:
///
/// * Complex types include `enum`, `struct`, `class`, tuple,
///   and function types.
/// * Recursive but not complex types are pointers, optionals,
///   and arrays.
/// * Placeholders are temporaries that stand in for named types.
/// * The rest of the types are called atomic or simple.
///   They include numeric types (bool, integral, floating-point),
///   Strings, Dates and Blobs.
/// * User-defined types are `enum`s, `struct`s and `class`es.
/// * Named types are also enums, structs and classes, along with
///   the built-in types (e.g. numeric types, strings, etc.).
/// * Product types are `struct`s, `class`es and tuples.
/// * Value types are enums, structs, and tuples, along with
///   the built-in types.
/// * Entity types are only classes (for now).
/// * Function types are… function types, obviously.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ComplexTypeKind {
    /// A complex type that is a value type: `struct`, `enum`, tuple
    Value,
    /// A complex type that is an entity type: currently, only `class`.
    Entity,
    /// A complex type that is a function type.
    Function,
}

/// Describes an enumerated type.
#[derive(Debug)]
pub struct EnumType {
    /// The name of the enum type.
    pub name: String,
    /// The list of variants: keys are variant names, each value
    /// is the type of the associated value of the variant.
    pub variants: BTreeMap<String, WkType>,
}

/// Describes a structure type.
#[derive(Debug)]
pub struct StructType {
    /// The name of the struct type.
    pub name: String,
    /// The list of fields: keys are the field names;
    /// each value is the type of the referred field.
    pub fields: BTreeMap<String, WkType>,
}

/// Describes a class type.
#[derive(Debug)]
pub struct ClassType {
    /// The name of the class type.
    pub name: String,
    /// The list of fields: keys are the field names;
    /// each value is the type of the referred field.
    pub fields: BTreeMap<String, WkType>,
}

/// Describes a function type.
#[derive(Debug, Clone)]
pub struct FunctionType {
    /// The list of types of each argument in order.
    pub arg_types: Vec<WkType>,
    /// The return type of the function.
    pub ret_type: WkType,
}

//
// Relations (also part of the Schema)
//

/// Represents one side of a relationship between two entities.
/// A particular `RelationSide` always describes the relation from
/// the point of view of one particular entity type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelationSide {
    /// Pointer to the type which forms this side of the relationship.
    pub entity: RcType,
    /// Name of the field this side uses to refer to the other side.
    pub field: Option<String>,
    /// Number of instances of this entity that may correspond to
    /// an instance in the other entity.
    pub cardinality: Cardinality,
}

/// The cardinality or multiplicity shows how many entities in the
/// referred side may be related to each entity in the referring side.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Cardinality {
    /// Optional relationship: an instance on this side may
    /// or may not correspond to an instance on the other side.
    ZeroOrOne,
    /// Required, unique relationship: exactly one instance on this
    /// side must correspond to an instance at the other side.
    One,
    /// Optional, multiple-valued relationship: any or no instances
    /// on this side may correspond to an instance on the other side.
    ZeroOrMore,
    /// Required, multiple-valued relationship: at least one instance
    /// on this side must correspond to an instance on the other side.
    OneOrMore,
}

/// An entity relationship is an **unordered** pair of relation sides.
#[derive(Debug, Clone)]
pub struct Relation {
    /// One side of the relationship.
    pub lhs: RelationSide,
    /// The other side of the relationship.
    pub rhs: RelationSide,
}

//
// Functions (Queries)
//

/// Expression: the core data structure.
/// This type is more or less a lambda calculus node on steroids.
#[derive(Debug)]
pub struct Expr {
    /// The type of the expression.
    pub ty: RcType, // can be Rc: no cycles are possible
    /// The value of the expression.
    pub value: Value,
    /// The identifier of the expression, unique within a top-level function.
    pub id: ExprId,
}

/// Represents the value of an expression node.
///
/// There are two kinds of values. One kind belongs in the Core:
/// only these variants are emitted directly by `SqirGen`.
/// They describe database operations as generally and as
/// uniformly as possible.
///
/// The other kind of value variants describe simpler,
/// specialized, frequently-encountered database operations.
/// These are only emitted by `SqirOpt`, as the result of
/// collapsing more general SQIR, so that the resulting code
/// is easier to map to actual database queries efficiently.
#[derive(Debug)]
pub enum Value {
    /// Value of a forward-declared function
    Placeholder,

    /// A nil constant.
    Nil,
    /// A Boolean constant.
    Bool(bool),
    /// An integral constant.
    Int(u64),
    /// A floating-point constant.
    Float(f64),
    /// A string constant.
    String(String),

    /// A function definition (~lambda expression).
    Function(Function),
    /// A function call (~application).
    Call(Call),
    /// A function argument. This has its own variant because it
    /// makes optimizations significantly easier.
    Argument {
        /// A back-pointer to the containing function.
        func: WkExpr,
        /// 0-based index of the argument within its containing function.
        index: usize,
    },
    /// A name reference (~substitution). The wrapped expression
    /// is the underlying expression referred to by the "variable".
    /// Variable declarations are implicit using named expressions.
    /// It must be weak, because the body of a named (eg. global)
    /// recursive function can refer to itself.
    Load(WkExpr),

    /// T -> T? conversion. Usually emitted from an implicit promotion.
    /// Each conversion generates a new temporary, therefore
    /// these conversions cannot form a reference cycle, so it's
    /// OK to make their associated value a strong pointer.
    OptionalWrap(RcExpr),
    /// Explicit T -> () conversion. Usually emitted by an expression statement.
    Ignore(RcExpr),

    /// Numeric unary negation operation.
    Neg(RcExpr),
    /// Addition operation.
    Add(RcExpr, RcExpr),
    /// Subtraction operation.
    Sub(RcExpr, RcExpr),
    /// Multiplication operation.
    Mul(RcExpr, RcExpr),
    /// Division operation.
    Div(RcExpr, RcExpr),
    /// Modulus (or remainder?) operation.
    Mod(RcExpr, RcExpr),

    /// Equality comparison.
    Eq(RcExpr, RcExpr),
    /// Inequality comparison.
    Neq(RcExpr, RcExpr),
    /// "Less than" comparison.
    Lt(RcExpr, RcExpr),
    /// "Less than or equal to" comparison.
    LtEq(RcExpr, RcExpr),
    /// "Greater than" comparison.
    Gt(RcExpr, RcExpr),
    /// "Greater than or equal to" comparison.
    GtEq(RcExpr, RcExpr),

    /// Logical conjunction.
    And(RcExpr, RcExpr),
    /// Logical disjunction.
    Or(RcExpr, RcExpr),
    /// Logical negation.
    Not(RcExpr),

    /// Set intersection.
    Intersect(RcExpr, RcExpr),
    /// Set union.
    Union(RcExpr, RcExpr),
    /// Set difference.
    SetDiff(RcExpr, RcExpr),

    /// Multi-way branch.
    Branch(Branch),
    /// An ordered sequence of expressions; a block.
    /// Evaluates each expression and yields the last one.
    Seq(Vec<RcExpr>),
    /// An array literal.
    Array(Vec<RcExpr>),
    /// A tuple literal.
    Tuple(Vec<RcExpr>),
    /// A struct literal. Keys are field names,
    /// values are the corresponding expressions.
    Struct(BTreeMap<String, RcExpr>),

    /// Generalized projection.
    Map(Map),
    /// Generalized aggregation.
    Reduce(Reduce),
    /// Generalized selection.
    Filter(Filter),
    /// Generalized sorting.
    Sort(Sort),
    /// Generalized grouping.
    Group(Group),
    /// A join operation between entities.
    /// TODO(H2CO3): design + implement this.
    Join,
    /// An insertion into some entities in the database.
    /// TODO(H2CO3): design + implement this.
    Insert,
    /// An update affecting some entities in the database.
    /// TODO(H2CO3): design + implement this.
    Update,

    /// Simple projection of one or more named columns of the given type.
    /// Only emitted by SQIROpt.
    Project(AttrRef),
    /// Simple grouping based on one or more named columns of the given type.
    /// Column names are in decreasing order of significance ("outer first").
    /// Only emitted by SQIROpt.
    GroupBy(AttrRef),
    /// Simple sorting based on one or more named columns of the given type.
    /// Column names are in decreasing order of (lexicographical) significance.
    /// Only emitted by SQIROpt.
    SortBy {
        /// The entity to sort.
        entity: RcType,
        /// Names of fields used as sort keys, in lexicographical order of importance.
        fields: Vec<(String, SortDirection)>
    },

    /// Project the sum of some attributes of a given type.
    /// Only emitted by SQIROpt.
    Sum(AttrRef),
    /// Project the average of some attributes of a given type.
    /// Only emitted by SQIROpt.
    Avg(AttrRef),
    /// Project the minimum of some attributes of a given type.
    /// Only emitted by SQIROpt.
    Min(AttrRef),
    /// Project the maximum of some attributes of a given type.
    /// Only emitted by SQIROpt.
    Max(AttrRef),
}

/// A reference to 0 or more attributes of a certain entity.
/// Used for simple, optimized operations (e.g. projections).
#[derive(Debug)]
pub struct AttrRef {
    /// The entity of which some fields should be examined.
    pub entity: RcType,
    /// The names of some fields of the entity.
    pub fields: Vec<String>,
}

/// A flag for indicating the direction of sorting.
/// The default is `Ascending`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SortDirection {
    /// Order smaller values before greater ones.
    Ascending,
    /// Order greater values before smaller ones.
    Descending,
}

impl Default for SortDirection {
    fn default() -> Self {
        SortDirection::Ascending
    }
}

/// `ExprId` is deliberately not `Clone`:
/// it should not be copied.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExprId {
    /// A temporary. The integer is its ID, unique within a global `fn`.
    Temp(usize),
    /// A global. (For now, a function.) Contained `String` is its name.
    Global(String),
}

/// A function. This is the value of global functions and closures.
#[derive(Debug, Clone)]
pub struct Function {
    /// Each function argument in order; `Argument` expressions.
    pub args: Vec<RcExpr>,
    /// The expression that is evaluated when the function is called.
    pub body: RcExpr,
}

/// A function call expression.
#[derive(Debug)]
pub struct Call {
    /// The function being called.
    pub callee: RcExpr,
    /// The list of actual arguments, in order.
    pub args: Vec<RcExpr>,
}

/// A multi-way branch expression. *The* way to do control flow.
#[derive(Debug)]
pub struct Branch {
    /// The expression that control flow will depend on.
    pub discriminant: RcExpr,
    /// The patterns that match the discriminant, along with their
    /// associated expression that will be evaluated upon match.
    pub arms: Vec<BranchArm>,
}

/// A match-value pair, representing one possible direction of control flow.
#[derive(Debug)]
pub struct BranchArm {
    /// The pattern that may match the discriminant.
    pub pattern: RcExpr,
    /// The expression to evaluate when `pattern` matches.
    pub value: RcExpr,
}

/// A general `map` or projection operation.
#[derive(Debug)]
pub struct Map {
    /// The sequence/collection which is the domain of the projection.
    pub domain: RcExpr,
    /// A unary function applied to every element of `domain`.
    pub op: RcExpr,
}

/// A general `reduce` or aggregation operation.
#[derive(Debug)]
pub struct Reduce {
    /// The domain of the aggregation (see `Map` too).
    pub domain: RcExpr,
    /// The identity element of the operation.
    pub zero: RcExpr,
    /// A binary operation on the next element and the previous partial result.
    pub op: RcExpr,
}

/// A general `filter` or selection operation.
#[derive(Debug)]
pub struct Filter {
    /// The domain of the selection (see `Map` too).
    pub domain: RcExpr,
    /// The unary, boolean-valued predicate applied to each element of the domain.
    pub pred: RcExpr,
}

/// A general sorting operation.
#[derive(Debug)]
pub struct Sort {
    /// The domain of the sorting operation.
    pub domain: RcExpr,
    /// Binary, boolean-valued comparison function for `LHS < RHS`.
    pub cmp: RcExpr,
}

/// A general grouping operation.
#[derive(Debug)]
pub struct Group {
    /// Elements to be grouped.
    pub domain: RcExpr,
    /// A function that returns a key that grouping will be based on.
    pub key: RcExpr,
}

/// Top-level type for wrapping SQIR for a complete program.
/// Field names are (hopefully) self-explanatory.
///
/// `*_types` fields are type caches: in LLVM's style, types
/// are interned and structurally uniqued, so that comparing
/// them for equality is a simple pointer comparison. This
/// requires them to be cached. Cache keys are chosen so that
/// they uniquely identify the structure of the type.
/// For named types, this is the name of the type; for wrapper
/// types (e.g. optionals), this is just the wrapped type, for
/// tuples, it is the product of the contained types, for decimals,
/// it's a pair of the number of integral and fractional bits, etc.
///
/// `relations` contains all existing relations between entities;
/// keys are `(referring entity, referring field name)` pairs.
///
/// `globals` stores all global values (e.g. top-level functions);
/// keys are namespaces/`impl` names: `None` for no namespace,
/// `Some("Name")` for `impl Name`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Sqir {
    /// Stores named builtins and user-defined types.
    pub named_types: BTreeMap<String, RcType>,
    /// Stores decimal types, parametrized by `(integral_digits, fractional_digits)`.
    pub decimal_types: HashMap<(usize, usize), RcType>,
    /// Stores cached optional types. Keys are the inner, wrapped types.
    pub optional_types: HashMap<RcType, RcType>,
    /// Stores cached pointer types. Keys are pointed types.
    pub pointer_types: HashMap<RcType, RcType>,
    /// Stores cached array types. Keys are element types.
    pub array_types: HashMap<RcType, RcType>,
    /// Stores cached tuple types. Keys are the product of contained types.
    pub tuple_types: HashMap<Vec<RcType>, RcType>,
    /// Stores cached function types. Keys are `([argument_types], return_type)`.
    pub function_types: HashMap<(Vec<RcType>, RcType), RcType>,
    /// Stores relations between entities. Keys are `(referring_entity, referring_field)`.
    pub relations: HashMap<(RcType, String), Relation>,
    /// Stores global values (currently, functions only). Keys are namespace
    /// (`impl`) names; `None` means top-level/free function, not in an `impl`.
    pub globals: BTreeMap<Option<String>, BTreeMap<String, RcExpr>>,
}


fn write_many_types(f: &mut Formatter, types: &[WkType]) -> fmt::Result {
    f.write_str("(")?;

    for (index, ty) in types.iter().enumerate() {
        let sep = if index > 0 { ", " } else { "" };
        write!(f, "{}{}", sep, ty)?
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
                write!(f, "({})?", wrapped)
            },
            Type::Pointer(ref pointed) => {
                write!(f, "&({})", pointed)
            },
            Type::Array(ref element) => {
                write!(f, "[{}]", element)
            },
            Type::Function(ref ty) => {
                write_many_types(f, &ty.arg_types)?;
                write!(f, " -> {}", ty.ret_type)
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

impl Display for PlaceholderKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            PlaceholderKind::Struct => "struct",
            PlaceholderKind::Class  => "class",
            PlaceholderKind::Enum   => "enum",
        };

        f.write_str(s)
    }
}

impl Display for Cardinality {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let s = match *self {
            Cardinality::ZeroOrOne  => "zero or one",
            Cardinality::One        => "one",
            Cardinality::ZeroOrMore => "zero or more",
            Cardinality::OneOrMore  => "one or more",
        };

        f.write_str(s)
    }
}

impl PartialOrd for RelationSide {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp(other).into()
    }
}

impl Ord for RelationSide {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_ref = self.entity.borrow().expect("self entity");
        let other_ref = other.entity.borrow().expect("other entity");

        let self_name = match *self_ref {
            Type::Class(ref c) => &c.name,
            _ => unreachable!("Non-Class class type?!"),
        };
        let other_name = match *other_ref {
            Type::Class(ref c) => &c.name,
            _ => unreachable!("Non-Class class type?!"),
        };

        let lhs = (self_name,  &self.field,  self.cardinality);
        let rhs = (other_name, &other.field, other.cardinality);

        lhs.cmp(&rhs)
    }
}

/// **NOTE:** Since an entity relationship is an **unordered** pair
/// of two relation sides, `==` has some additional symmetry:
///
/// `Relation { lhs: S1, rhs: S2 } == Relation { lhs: S2, rhs: S1 }`
///
/// yields `true` for any two `RelationSide`s `S1` and `S2`.
impl PartialEq for Relation {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Relation {}

impl PartialOrd for Relation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp(other).into()
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

impl Sqir {
    /// Creates an "empty" SQIR value, populated with builtins only.
    pub fn new() -> Self {
        let named_types = btree_map!{
            BUILTIN_NAME.bool_name   => Type::Bool,
            BUILTIN_NAME.int_name    => Type::Int,
            BUILTIN_NAME.float_name  => Type::Float,
            BUILTIN_NAME.string_name => Type::String,
            BUILTIN_NAME.blob_name   => Type::Blob,
            BUILTIN_NAME.date_name   => Type::Date,
        };

        Sqir {
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

    /// Relations can be stored either once or twice in the
    /// `relations` field (because they need to be associated
    /// with named sides for easy querygen), but schemagen
    /// still requires a non-redundant set of relations.
    /// By convention, `relations` contains relations in a
    /// way that the RelationSide corresponding to the key
    /// appears on the LHS, whereas the referred RelationSide
    /// is the RHS.
    pub fn unique_relations(&self) -> BTreeSet<&Relation> {
        self.relations.values().collect()
    }
}

/// Returns the same empty SQIR object as `Sqir::new()`.
impl Default for Sqir {
    fn default() -> Self {
        Sqir::new()
    }
}
