//
// dalgen/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/06/2017
//

//! DALGen, the backend of the PHiLe compiler. This is the part
//! that generates actual code in your programming language of
//! choice for a Database Abstraction Layer from optimized SQIR.

mod rust;
mod c;
mod cxx;
mod objc;
mod swift;
mod go;
mod js;
mod python;
mod java;

use std::io;
use std::rc::Rc;
use std::cell::RefCell;
use heck::{ SnakeCase, ShoutySnakeCase, MixedCase, CamelCase };
use error::Result;
use sqir::*;


/// The database engine flavor for which a Database Abstraction Layer will be generated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DatabaseEngine {
    /// SQLite, a very lightweight, embeddable, relational database.
    SQLite3,
    /// MongoDB, a JSON document store.
    MongoDB,
    /// MariaDB, a GPL fork of the MySQL RMDBS.
    MariaDB,
}

/// The programming language in which the DAL will be implemented.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Language {
    /// Rust 1.x.
    Rust,
    /// C. 89/90? 99? 11? I don't know yet.
    C,
    /// Modern C++ (11, 14, 17).
    CXX,
    /// Modern Objective-C (2.0).
    ObjectiveC,
    /// Swift. I don't write a version number because it will change anyway.
    Swift,
    /// Go 1.x.
    Go,
    /// ECMAScript, if we are being technical.
    JavaScript,
    /// Python. 2 or 3? I don't know yet.
    Python,
    /// Java. Yeah, seriously.
    Java,
}

/// The strategy with which the generated DAL will query the DB.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DatabaseAccessMode {
    /// POD, or Plain Old Object: objects returned from the DAL are **immutable** and do **not**
    /// automatically synchronize their state with the DB. This may be better for performance.
    Pod,
    /// Active Records: objects returned by the DAL are **mutable** and they **automatically
    /// synchronize** their state with the DB. May be better for application-level consistency.
    ActiveRecord,
}

/// The `default()` database access mode is POD.
impl Default for DatabaseAccessMode {
    fn default() -> Self {
        DatabaseAccessMode::Pod
    }
}

/// The rewriting strategy applied to various kinds of named program elements,
/// such as function, variable, and type names.
/// In the `CodegenParams` struct, optional instances of this transform are
/// specified, where `None` means "default for the programming language".
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NameTransform {
    /// Don't touch it!
    Identity,
    /// `lower_snake_case`. Isn't SnakeCase spelled with camel case ironic, by the way?
    LowerSnakeCase,
    /// `UPPER_SNAKE_CASE`. Use this only when you are frustrated and want to yell.
    UpperSnakeCase,
    /// `lowerCamelCase`.
    LowerCamelCase,
    /// `UpperCamelCase`, also known as `PascalCase`.
    UpperCamelCase,
}

/// A bunch of centralized settings governing the behavior of DALGen.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CodegenParams {
    /// The database flavor to be targeted. See the docs of `DatabaseEngine`.
    pub database: DatabaseEngine,
    /// The programming language to be targeted. See the docs of `Language`.
    pub language: Language,
    /// Database access strategy. See the docs for `DatabaseAccessMode`.
    pub database_access_mode: Option<DatabaseAccessMode>,
    /// Namespace name. This will be used in different ways for different
    /// programming languages. For example, in Go, it will be the package
    /// name, and is thus **mandatory**.
    pub namespace: Option<String>,
    /// The transform to be applied to names of user-defined types.
    pub type_name_transform: Option<NameTransform>,
    /// The transform to be applied to fields of `struct`s and `class`es.
    pub field_name_transform: Option<NameTransform>,
    /// The transform to be applied to variants of `enum`s.
    pub variant_name_transform: Option<NameTransform>,
    /// The transform to be applied to names of user-defined functions.
    pub func_name_transform: Option<NameTransform>,
    /// The transform to be applied to the name of the namespace.
    pub namespace_transform: Option<NameTransform>,
}

/// Functions of this type are expected to yield a (possibly cached)
/// `io::Write` object that DALGen can write to. The cache key,
/// specified as the string parameter of the function, is sometimes
/// a file name derived from the `impl` of a user-defined type.
///
/// TODO(H2CO3): rewrite this using `RcCell` once custom smart pointers
/// can point to trait objects, i.e. when `CoerceUnsized` and `Unsize`
/// are stabilized. See [issue #27732](https://github.com/rust-lang/rust/issues/27732).
pub type WriterProvider = FnMut(&str) -> Result<Rc<RefCell<io::Write>>>;


/// Given the SQIR representation of a program, and some configuration parameters,
/// generates a Database Abstraction Layer and writes the code into `io::Write`s.
///
/// # Arguments:
///
/// * `sqir`: Optimized SQIR intermediate representation for a PHiLe program.
/// * `params`: code generation parameters; see the docs for `CodegenParams`.
/// * `wp`: a provider of `io::Write`s that will accumulate generated code.
///
/// # Return value:
///
/// * `Ok(())`, if code generation completed successfully.
/// * `Err(Error)`, if an error occurred. This is typically an I/O error (`Error::IO`)
///   or an `Error::Semantic` induced by some special requirement of the backend
///   (e.g. a missing namespace when generating Go code).
pub fn generate_dal(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    use self::Language::*;

    match params.language {
        Rust       => rust  ::generate_dal(sqir, params, wp),
        C          => c     ::generate_dal(sqir, params, wp),
        CXX        => cxx   ::generate_dal(sqir, params, wp),
        ObjectiveC => objc  ::generate_dal(sqir, params, wp),
        Swift      => swift ::generate_dal(sqir, params, wp),
        Go         => go    ::generate_dal(sqir, params, wp),
        JavaScript => js    ::generate_dal(sqir, params, wp),
        Python     => python::generate_dal(sqir, params, wp),
        Java       => java  ::generate_dal(sqir, params, wp),
    }
}

//
// Name Transforms
//

fn transform_type_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.type_name_transform,
        params.language,
        default_type_name_transform,
    )
}

fn transform_field_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.field_name_transform,
        params.language,
        default_field_name_transform,
    )
}

fn transform_variant_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.variant_name_transform,
        params.language,
        default_variant_name_transform,
    )
}

fn transform_func_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.func_name_transform,
        params.language,
        default_func_name_transform,
    )
}

fn transform_namespace(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.namespace_transform,
        params.language,
        default_namespace_transform,
    )
}

fn transform_name<D>(
    name: &str,
    transform: Option<NameTransform>,
    lang: Language,
    default: D,
) -> String
    where D: FnOnce(Language) -> NameTransform {

    use self::NameTransform::*;

    match transform.unwrap_or(default(lang)) {
        Identity       => name.to_owned(),
        LowerSnakeCase => name.to_snake_case(),
        UpperSnakeCase => name.to_shouty_snake_case(),
        LowerCamelCase => name.to_mixed_case(),
        UpperCamelCase => name.to_camel_case(),
    }
}

fn default_type_name_transform(lang: Language) -> NameTransform {
    use self::Language::*;
    use self::NameTransform::*;

    match lang {
        Rust       => UpperCamelCase,
        C          => UpperCamelCase,
        CXX        => UpperCamelCase,
        ObjectiveC => UpperCamelCase,
        Swift      => UpperCamelCase,
        Go         => UpperCamelCase,
        JavaScript => UpperCamelCase,
        Python     => UpperCamelCase,
        Java       => UpperCamelCase,
    }
}

fn default_field_name_transform(lang: Language) -> NameTransform {
    use self::Language::*;
    use self::NameTransform::*;

    match lang {
        Rust       => LowerSnakeCase,
        C          => LowerSnakeCase,
        CXX        => LowerSnakeCase,
        ObjectiveC => LowerCamelCase,
        Swift      => LowerCamelCase,
        Go         => UpperCamelCase,
        JavaScript => LowerCamelCase,
        Python     => LowerSnakeCase,
        Java       => LowerCamelCase,
    }
}

fn default_variant_name_transform(lang: Language) -> NameTransform {
    use self::Language::*;
    use self::NameTransform::*;

    match lang {
        Rust       => UpperCamelCase,
        C          => UpperCamelCase,
        CXX        => UpperCamelCase,
        ObjectiveC => UpperCamelCase,
        Swift      => LowerCamelCase, // new Swift enums suck :-(
        Go         => UpperCamelCase,
        JavaScript => UpperCamelCase,
        Python     => UpperCamelCase, // I'm _not_ doing ALL_CAPS.
        Java       => UpperCamelCase, // Not even for Java.
    }
}

fn default_func_name_transform(lang: Language) -> NameTransform {
    use self::Language::*;
    use self::NameTransform::*;

    match lang {
        Rust       => LowerSnakeCase,
        C          => LowerSnakeCase,
        CXX        => LowerSnakeCase,
        ObjectiveC => LowerCamelCase,
        Swift      => LowerCamelCase,
        Go         => UpperCamelCase,
        JavaScript => LowerCamelCase,
        Python     => LowerSnakeCase,
        Java       => LowerCamelCase,
    }
}

fn default_namespace_transform(lang: Language) -> NameTransform {
    use self::Language::*;
    use self::NameTransform::*;

    match lang {
        Rust       => Identity,
        C          => Identity,
        CXX        => Identity,
        ObjectiveC => Identity,
        Swift      => Identity,
        Go         => Identity,
        JavaScript => Identity,
        Python     => Identity,
        Java       => Identity,
    }
}
