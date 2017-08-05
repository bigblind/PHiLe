//
// codegen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 01/06/2017
//

use std::io;
use std::rc::Rc;
use std::cell::RefCell;
use heck::{ SnakeCase, ShoutySnakeCase, MixedCase, CamelCase };
use error::Result;


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DatabaseEngine {
    SQLite3,
    MongoDB,
    MariaDB,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Language {
    Rust,
    C,
    CXX,
    ObjectiveC,
    Swift,
    Go,
    JavaScript,
    Python,
    Java,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DatabaseAccessMode {
    Pod,
    ActiveRecord,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NameTransform {
    Identity,
    LowerSnakeCase, // isn't SnakeCase spelled with camel case ironic?
    UpperSnakeCase,
    LowerCamelCase,
    UpperCamelCase,
}

// TODO(H2CO3): rewrite this using RcCell once custom smart pointers
//              can point to trait objects, i.e. when CoerceUnsized
//              and Unsize are stabilized (see issue #27732)
pub type WriterProvider = FnMut(&str) -> Result<Rc<RefCell<io::Write>>>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CodegenParams {
    pub database:               DatabaseEngine,
    pub language:               Language,
    pub database_access_mode:   DatabaseAccessMode,
    pub namespace:              Option<String>,
    pub type_name_transform:    Option<NameTransform>,
    pub field_name_transform:   Option<NameTransform>,
    pub variant_name_transform: Option<NameTransform>,
    pub func_name_transform:    Option<NameTransform>,
    pub namespace_transform:    Option<NameTransform>,
}

//
// Name Transforms
//

pub fn transform_type_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.type_name_transform,
        params.language,
        default_type_name_transform
    )
}

pub fn transform_field_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.field_name_transform,
        params.language,
        default_field_name_transform
    )
}

pub fn transform_variant_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.variant_name_transform,
        params.language,
        default_variant_name_transform
    )
}

pub fn transform_func_name(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.func_name_transform,
        params.language,
        default_func_name_transform
    )
}

pub fn transform_namespace(name: &str, params: &CodegenParams) -> String {
    transform_name(
        name,
        params.namespace_transform,
        params.language,
        default_namespace_transform
    )
}

fn transform_name<D>(
    name: &str,
    transform: Option<NameTransform>,
    lang: Language,
    default: D
) -> String
    where D: FnOnce(Language) -> NameTransform {

    match transform.unwrap_or(default(lang)) {
        NameTransform::Identity       => name.to_owned(),
        NameTransform::LowerSnakeCase => name.to_snake_case(),
        NameTransform::UpperSnakeCase => name.to_shouty_snake_case(),
        NameTransform::LowerCamelCase => name.to_mixed_case(),
        NameTransform::UpperCamelCase => name.to_camel_case(),
    }
}

fn default_type_name_transform(lang: Language) -> NameTransform {
    match lang {
        Language::Rust       => NameTransform::UpperCamelCase,
        Language::C          => NameTransform::UpperCamelCase,
        Language::CXX        => NameTransform::UpperCamelCase,
        Language::ObjectiveC => NameTransform::UpperCamelCase,
        Language::Swift      => NameTransform::UpperCamelCase,
        Language::Go         => NameTransform::UpperCamelCase,
        Language::JavaScript => NameTransform::UpperCamelCase,
        Language::Python     => NameTransform::UpperCamelCase,
        Language::Java       => NameTransform::UpperCamelCase,
    }
}

fn default_field_name_transform(lang: Language) -> NameTransform {
    match lang {
        Language::Rust       => NameTransform::LowerSnakeCase,
        Language::C          => NameTransform::LowerSnakeCase,
        Language::CXX        => NameTransform::LowerSnakeCase,
        Language::ObjectiveC => NameTransform::LowerCamelCase,
        Language::Swift      => NameTransform::LowerCamelCase,
        Language::Go         => NameTransform::UpperCamelCase,
        Language::JavaScript => NameTransform::LowerCamelCase,
        Language::Python     => NameTransform::LowerSnakeCase,
        Language::Java       => NameTransform::LowerCamelCase,
    }
}

fn default_variant_name_transform(lang: Language) -> NameTransform {
    match lang {
        Language::Rust       => NameTransform::UpperCamelCase,
        Language::C          => NameTransform::UpperCamelCase,
        Language::CXX        => NameTransform::UpperCamelCase,
        Language::ObjectiveC => NameTransform::UpperCamelCase,
        Language::Swift      => NameTransform::LowerCamelCase, // new Swift enums suck :-(
        Language::Go         => NameTransform::UpperCamelCase,
        Language::JavaScript => NameTransform::UpperCamelCase,
        Language::Python     => NameTransform::UpperCamelCase, // I'm _not_ doing ALL_CAPS.
        Language::Java       => NameTransform::UpperCamelCase, // Not even for Java.
    }
}

fn default_func_name_transform(lang: Language) -> NameTransform {
    match lang {
        Language::Rust       => NameTransform::LowerSnakeCase,
        Language::C          => NameTransform::LowerSnakeCase,
        Language::CXX        => NameTransform::LowerSnakeCase,
        Language::ObjectiveC => NameTransform::LowerCamelCase,
        Language::Swift      => NameTransform::LowerCamelCase,
        Language::Go         => NameTransform::UpperCamelCase,
        Language::JavaScript => NameTransform::LowerCamelCase,
        Language::Python     => NameTransform::LowerSnakeCase,
        Language::Java       => NameTransform::LowerCamelCase,
    }
}

fn default_namespace_transform(lang: Language) -> NameTransform {
    match lang {
        Language::Rust       => NameTransform::Identity,
        Language::C          => NameTransform::Identity,
        Language::CXX        => NameTransform::Identity,
        Language::ObjectiveC => NameTransform::Identity,
        Language::Swift      => NameTransform::Identity,
        Language::Go         => NameTransform::Identity,
        Language::JavaScript => NameTransform::Identity,
        Language::Python     => NameTransform::Identity,
        Language::Java       => NameTransform::Identity,
    }
}
