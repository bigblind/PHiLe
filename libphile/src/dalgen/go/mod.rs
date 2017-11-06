//
// dalgen/go/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 18/07/2017
//

use std::io;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{ self, Write, Formatter, Display };
use error::{ Error, Result };
use dalgen::*;
use sqir::*;
use util::*;

mod sqlite3;
mod mongodb;
mod mariadb;


//
// Naming Conventions for frequently-occurring types and variables
//

#[derive(Debug, Clone, Copy)]
struct NamingConvention {
    context_type: &'static str,
    context_name: &'static str,
    tmp_prefix:   &'static str,
    top_basename: &'static str,
}

static NAMING_CONVENTION: NamingConvention = NamingConvention {
    context_type: "PhileCtx",
    context_name: "ctx",
    tmp_prefix:   "tmp_",
    top_basename: "PHiLe-Context",
};

//
// Public generator
//

pub fn generate_dal(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    match params.database_access_mode.unwrap_or_default() {
        DatabaseAccessMode::Pod          => generate_pod(sqir, params, wp),
        DatabaseAccessMode::ActiveRecord => generate_active_record(sqir, params, wp),
    }
}

//
// Top-level generators: POD and ActiveRecord
//

fn generate_pod(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    // First, generate POD type definitions
    PodUdTypeGen { sqir, params, wp }.generate()?;

    // Then, generate the schema
    match params.database {
        DatabaseEngine::SQLite3 => sqlite3::generate_schema(sqir, params, wp)?,
        DatabaseEngine::MongoDB => mongodb::generate_schema(sqir, params, wp)?,
        DatabaseEngine::MariaDB => mariadb::generate_schema(sqir, params, wp)?,
    }

    // Finally, generate POD-compatible queries
    generate_query_pod(sqir, params, wp)
}

fn generate_active_record(_sqir: &Sqir, _params: &CodegenParams, _wp: &mut WriterProvider) -> Result<()> {
    unimplemented!()
}

//
// State for generating user-defined type definitions,
// to be used with the Plain Old Data DB access mode.
//

struct PodUdTypeGen<'a> {
    sqir:   &'a Sqir,
    params: &'a CodegenParams,
    wp:     &'a mut WriterProvider,
}

impl<'a> PodUdTypeGen<'a> {
    fn generate(mut self) -> Result<()> {
        let wrs = self.writers_for_types(self.sqir.named_types.iter())?;

        for (name, ty) in &self.sqir.named_types {
            // primitive named types have no associated writer
            let mut wr = match wrs.get(name) {
                Some(w) => w.try_borrow_mut()?,
                None    => continue,
            };

            match *ty.borrow()? {
                Type::Struct(ref st) => self.write_fields(&mut *wr, name, &st.fields)?,
                Type::Class(ref ct)  => self.write_fields(&mut *wr, name, &ct.fields)?,
                Type::Enum(ref et)   => self.write_variants(&mut *wr, name, &et.variants)?,
                ref t => bug!("Attempt to declare primitive type {}?!", t)?,
            }
        }

        Ok(())
    }

    fn writers_for_types<'b, I>(&mut self, types: I) -> Result<BTreeMap<String, Rc<RefCell<io::Write>>>>
        where I: Iterator<Item=(&'b String, &'b RcType)> {

        let mut writers = BTreeMap::new();

        for (name, ty) in types {
            // primitive named types need no declaration
            match *ty.borrow()? {
                Type::Struct(_) | Type::Class(_) | Type::Enum(_) => {},
                _ => continue,
            }

            writers.insert(name.clone(), self.writer_with_preamble(name)?);
        }

        Ok(writers)
    }

    fn writer_with_preamble(&mut self, name: &str) -> Result<Rc<RefCell<io::Write>>> {
        let file_name = name.to_owned() + ".go";
        let wptr = (self.wp)(&file_name)?;
        write_header(&mut *wptr.try_borrow_mut()?, self.params)?;
        Ok(wptr)
    }

    // TODO(H2CO3): refactor
    fn write_fields(
        &self,
        wr: &mut io::Write,
        raw_struct_name: &str,
        fields: &BTreeMap<String, WkType>,
    ) -> Result<()> {
        // Respect the type name transform
        let struct_name = transform_type_name(raw_struct_name, self.params);
        writeln!(wr, "type {} struct {{", struct_name)?;

        // Respect the field name transform
        // TODO(H2CO3): For efficiency, sort fields by alignment.
        let transformed_fields: Vec<_> = fields.iter().map(
            |(name, ty)| (transform_field_name(name, self.params), ty)
        ).collect();

        let max_len = transformed_fields.iter()
            .map(|&(ref name, _)| grapheme_count(name))
            .max().unwrap_or(0);

        let pad = " ".repeat(max_len);

        // Indexing `pad` with grapheme counts is safe because it's ASCII-only.
        for (name, ty) in transformed_fields {
            write!(wr, "    {}{} ", name, &pad[grapheme_count(&name)..])?;
            write_type(wr, ty, self.params)?;
            writeln!(wr)?;
        }

        writeln!(wr, "}}\n").map_err(From::from)
    }

    // TODO(H2CO3): refactor
    fn write_variants(
        &self,
        wr: &mut io::Write,
        raw_enum_name: &str,
        variants: &BTreeMap<String, WkType>,
    ) -> io::Result<()> {
        // Respect the type name transform
        let enum_name = transform_type_name(raw_enum_name, self.params);

        // TODO(H2CO3): if none of the variants has an
        // associated value (every variant has type ()),
        // then just define the enum type as `string`.
        // TODO(H2CO3): let the user choose whether
        // enum discriminators are strings or ints, for
        // a tradeoff between speed and maintainability.
        // TODO(H2CO3): transform these field names...?!
        writeln!(wr, "type {} struct {{", enum_name)?;
        writeln!(wr, "    Variant string")?;
        writeln!(wr, "    Value   interface{{}}")?;
        writeln!(wr, "}}\n")?;

        // Respect the variant name transform.
        // For correctly prefixing each variant name, first
        // they are prefixed with the name of the variant and
        // an underscore, which is a word boundary according
        // to the case transforms in the Heck crate. Then, the
        // composed 'raw' variant name is transformed to obtain
        // the final, correctly cased and prefixed variant name.
        let transformed_variants: Vec<_> = variants.keys().map(|vname| {
            let variant_name = raw_enum_name.to_owned() + "_" + vname;
            transform_variant_name(&variant_name, self.params)
        }).collect();

        let max_len = transformed_variants.iter().map(String::len).max().unwrap_or(0);
        let pad = " ".repeat(max_len);

        writeln!(wr, "const (")?;

        for vname in &transformed_variants {
            writeln!(wr, "    {}{} = \"{}\"", vname, &pad[grapheme_count(vname)..], vname)?;
        }

        writeln!(wr, ")\n")
    }
}
//
// Type annotations for declarations, etc.
//

fn write_type(wr: &mut io::Write, ty: &WkType, params: &CodegenParams) -> Result<()> {
    let rc = ty.to_rc()?;
    let ptr = rc.borrow()?;

    match *ptr {
        Type::Bool  => write!(wr, "bool")?,
        Type::Int   => write!(wr, "int64")?,
        Type::Float => write!(wr, "float64")?,
        Type::Decimal { .. } => unimplemented!(),

        Type::String => write!(wr, "string")?,
        Type::Blob   => write!(wr, "[]byte")?,
        Type::Date   => write!(wr, "time.Time")?,

        Type::Optional(ref wrapped) => write_optional_type(wr, wrapped, params)?,
        Type::Pointer(ref pointed)  => write_pointer_type(wr, pointed, params)?,
        Type::Array(ref element)    => write_array_type(wr, element, params)?,
        Type::Tuple(ref types)      => write_tuple_type(wr, types, params)?,

        // Respect type name transform
        Type::Enum(ref et)   => write!(wr, "{}", transform_type_name(&et.name, params))?,
        Type::Struct(ref st) => write!(wr, "{}", transform_type_name(&st.name, params))?,
        Type::Class(ref ct)  => write!(wr, "{}", transform_type_name(&ct.name, params))?,

        Type::Function(ref ft) => write_function_type(wr, ft, params)?,
        Type::Placeholder { ref name, kind, .. } => bug!(
            "Unresolved Placeholder({}, {})", name, kind
        )?,
    }

    Ok(())
}

fn write_optional_type(wr: &mut io::Write, wrapped: &WkType, params: &CodegenParams) -> Result<()> {
    write_pointer_type(wr, wrapped, params)
}

fn write_pointer_type(wr: &mut io::Write, pointed: &WkType, params: &CodegenParams) -> Result<()> {
    write!(wr, "*").map_err(From::from).and_then(|_| write_type(wr, pointed, params))
}

fn write_array_type(wr: &mut io::Write, element: &WkType, params: &CodegenParams) -> Result<()> {
    write!(wr, "[]").map_err(From::from).and_then(|_| write_type(wr, element, params))
}

fn write_tuple_type(wr: &mut io::Write, types: &[WkType], params: &CodegenParams) -> Result<()> {
    write!(wr, "struct {{ ")?;

    for (idx, ty) in types.iter().enumerate() {
        write!(wr, "F{} ", idx)?;
        write_type(wr, ty, params)?;
        write!(wr, "; ")?;
    }

    write!(wr, "}}").map_err(From::from)
}

fn write_function_type(wr: &mut io::Write, ty: &FunctionType, params: &CodegenParams) -> Result<()> {
    write!(wr, "func(")?;

    for arg in &ty.arg_types {
        write_type(wr, arg, params)?;
        write!(wr, ", ")?;
    }

    write!(wr, ") ")?;

    write_type(wr, &ty.ret_type, params).map_err(From::from)
}

//
// Package/namespace, include and dummy type uses
//

fn write_header(wr: &mut io::Write, params: &CodegenParams) -> Result<()> {
    write_comment_header(wr)?;
    write_namespace(wr, params)?;
    write_imports(wr, params)?;
    write_dummy_uses(wr, params)?;
    Ok(())
}

fn write_comment_header(wr: &mut io::Write) -> io::Result<()> {
    writeln!(wr, "//")?;
    writeln!(wr, "// Generated by PHiLe v{}", PACKAGE_INFO.version)?;
    writeln!(wr, "// Copyright (C) 2017, {}", PACKAGE_INFO.authors)?;
    writeln!(wr, "//")?;
    writeln!(wr)
}

// Namespace <-> package name
// Respect namespace transform
fn write_namespace(wr: &mut io::Write, params: &CodegenParams) -> Result<()> {
    params.namespace.as_ref().map(
        |ns| transform_namespace(ns, params)
    ).ok_or_else(|| Error::Semantic {
        message: "Missing namespace".to_owned(),
        range:   None,
    }).and_then(
        |ns| writeln!(wr, "package {}\n", ns).map_err(From::from)
    )
}

fn write_imports(wr: &mut io::Write, params: &CodegenParams) -> io::Result<()> {
    // stdlib imports
    writeln!(wr, "import \"time\"")?;

    // driver-specific imports
    match params.database {
        DatabaseEngine::MongoDB => writeln!(wr, "import \"gopkg.in/mgo.v2\"")?,
        DatabaseEngine::MariaDB => unimplemented!(),
        DatabaseEngine::SQLite3 => unimplemented!(),
    }

    writeln!(wr)
}

fn write_dummy_uses(wr: &mut io::Write, params: &CodegenParams) -> io::Result<()> {
    // stdlib stuff is always imported and should be used
    writeln!(wr, "type _ time.Time")?;

    // driver imports are conditional, so are their dummy uses
    match params.database {
        DatabaseEngine::MongoDB => writeln!(wr, "type _ mgo.Session")?,
        DatabaseEngine::MariaDB => unimplemented!(),
        DatabaseEngine::SQLite3 => unimplemented!(),
    }

    writeln!(wr)
}

//
// Escaping string literals
//

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
struct EscapedStr<'a>(&'a str);

impl<'a> Display for EscapedStr<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_char('"')?;

        for ch in self.0.chars() {
            match ch {
                '\"' => f.write_str(r#"\""#)?,
                '\\' => f.write_str(r"\\")?,
                '\n' => f.write_str(r"\n")?,
                '\r' => f.write_str(r"\r")?,
                '\t' => f.write_str(r"\t")?,
                '\x20'...'\x7e' => f.write_char(ch)?, // printable
                '\u{0000}'...'\u{ffff}' => write!(f, r"\u{:04x}", ch as u32)?,
                _ => write!(f, r"\U{:08X}", ch as u32)?,
            }
        }

        f.write_char('"')
    }
}

//
// Top-level (global func + impl) generators
//

fn generate_query_pod(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    for (ns_name, namespace) in &sqir.globals {
        for (global_name, expr) in namespace {
            let ns_name = ns_name.as_ref().map(String::as_str);
            let base_name = ns_name.unwrap_or(NAMING_CONVENTION.top_basename);
            let file_name = base_name.to_owned() + ".go";
            let wr = wp(&file_name)?;

            generate_global(
                &mut *wr.try_borrow_mut()?,
                ns_name,
                global_name,
                &*expr.borrow()?,
                params
            )?;
        }
    }

    Ok(())
}

fn generate_global(
    wr:     &mut io::Write,
    ns:     Option<&str>,
    name:   &str,
    expr:   &Expr,
    params: &CodegenParams,
) -> Result<()> {
    match (&expr.value, &*expr.ty.borrow()?) {
        (&Value::Function(ref func), &Type::Function(ref ty)) => {
            generate_function(wr, ns, name.into(), func, ty, params)
        },
        (val, ty) => bug!("Non-Function global?! {} ({:#?})", ty, val),
    }
}

//
// Expression Generators
//

fn generate_expr(wr: &mut io::Write, expr: &RcExpr, params: &CodegenParams) -> Result<()> {
    let ptr = expr.borrow()?;

    write!(wr, "    var ")?;
    write_expr_decl(wr, expr, params)?;
    writeln!(wr)?;

    // Always use the expression by reading its value.
    // so that the damn compiler shuts up about unused variables.
    // This is necessary in the case of user-defined variables
    // as well as compiler-generated temporaries, as the latter
    // may be unused too due to ignored temporaries (eg.: 42;).
    // This does _not_ generate dummy uses for function arguments,
    // as function arguments are not serialized by generate_expr().
    // That's OK though, as unused arguments are not an error in Go.
    write!(wr, "    _ = ")?;
    write_expr_id(wr, &ptr.id, params)?;
    writeln!(wr)?;

    match ptr.value {
        Value::Placeholder      => bug!("Placeholder should have been replaced"),
        Value::Nil              => generate_nil_literal(wr, &ptr.id, params),
        Value::Bool(b)          => generate_bool_literal(wr, &ptr.id, b, params),
        Value::Int(n)           => generate_int_literal(wr, &ptr.id, n, params),
        Value::Float(x)         => generate_float_literal(wr, &ptr.id, x, params),
        Value::String(ref s)    => generate_string_literal(wr, &ptr.id, s, params),
        Value::Load(ref expr)   => generate_load(wr, &ptr.id, expr, params),
        Value::OptionalWrap(_)  => unimplemented!(),
        Value::Ignore(ref expr) => generate_ignore(wr, &ptr.id, expr, params),
        Value::Seq(ref exprs)   => generate_sequence(wr, &ptr.id, exprs, params),
        _ => unimplemented!(),
    }
}

fn generate_nil_literal(wr: &mut io::Write, id: &ExprId, params: &CodegenParams) -> Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    writeln!(wr, " = nil")?;
    Ok(())
}

fn generate_bool_literal(wr: &mut io::Write, id: &ExprId, b: bool, params: &CodegenParams) -> Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    writeln!(wr, " = {}", b)?;
    Ok(())
}

fn generate_int_literal(wr: &mut io::Write, id: &ExprId, n: u64, params: &CodegenParams) -> Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    writeln!(wr, " = {}", n)?;
    Ok(())
}

fn generate_float_literal(wr: &mut io::Write, id: &ExprId, x: f64, params: &CodegenParams) -> Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    writeln!(wr, " = {}", x)?;
    Ok(())
}

fn generate_string_literal(wr: &mut io::Write, id: &ExprId, s: &str, params: &CodegenParams) -> Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    writeln!(wr, " = {}", EscapedStr(s))?;
    Ok(())
}

fn generate_load(wr: &mut io::Write, id: &ExprId, expr: &WkExpr, params: &CodegenParams) -> Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    write!(wr, " = ")?;
    let rc = expr.to_rc()?;
    write_expr_id(wr, &rc.borrow()?.id, params)?;
    writeln!(wr)?;
    Ok(())
}

fn generate_ignore(wr: &mut io::Write, id: &ExprId, expr: &RcExpr, params: &CodegenParams) -> Result<()> {
    generate_expr(wr, expr, params)?;
    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    writeln!(wr, " = struct{{}}{{}}")?;
    Ok(())
}

fn generate_sequence(wr: &mut io::Write, id: &ExprId, exprs: &[RcExpr], params: &CodegenParams) -> Result<()> {
    for expr in exprs {
        generate_expr(wr, expr, params)?
    }

    write!(wr, "    ")?;
    write_expr_id(wr, id, params)?;
    write!(wr, " = ")?;

    match exprs.last() {
        Some(expr) => {
            write_expr_id(wr, &expr.borrow()?.id, params)?;
            writeln!(wr).map_err(From::from)
        },
        None => writeln!(wr, "struct{{}}{{}}").map_err(From::from),
    }
}

fn generate_function(
    wr:     &mut io::Write,
    ns:     Option<&str>,
    name:   Option<&str>,
    func:   &Function,
    ty:     &FunctionType,
    params: &CodegenParams,
) -> Result<()> {
    // lambdas can't be in a namespace, only global functions can
    assert!(name.is_some() || ns.is_none());

    write!(wr, "func")?;

    if let Some(func_name) = name {
        write!(
            wr,
            " ({} {}) {}",
            NAMING_CONVENTION.context_name,
            NAMING_CONVENTION.context_type,
            name_for_global_func(ns, func_name, params),
        )?
    }

    write!(wr, "(")?;

    for arg in &func.args {
        let ptr = arg.borrow()?;
        match ptr.value {
            Value::Argument { .. } => match ptr.id {
                ExprId::Temp(_) => {},
                ExprId::Global(ref name) => bug!("Argument is global {}?!", name)?,
            },
            ref val => bug!("Non-Argument argument?! {:#?}", val)?,
        }

        write_expr_decl(wr, arg, params)?;
        write!(wr, ", ")?;
    }

    write!(wr, ") ")?;
    write_type(wr, &ty.ret_type, params)?;
    writeln!(wr, " {{")?;
    generate_expr(wr, &func.body, params)?;
    write!(wr, "    return ")?;
    write_expr_id(wr, &func.body.borrow()?.id, params)?;
    writeln!(wr)?;
    writeln!(wr, "}}")?;
    writeln!(wr).map_err(From::from)
}

// The following functions write references (i.e. reads/loads) and
// variable/argument declarations for named-by-the-user variable
// bindings and temporary expressions.
// TODO(H2CO3): Once global IDs gain a namespace name, respect that as well.
// TODO(H2CO3): What to do with non-function globals (e.g. enum variants)?
fn write_expr_id(wr: &mut io::Write, id: &ExprId, params: &CodegenParams) -> io::Result<()> {
    match *id {
        ExprId::Temp(index)      => write!(wr, "{}{}", NAMING_CONVENTION.tmp_prefix, index),
        ExprId::Global(ref name) => write!(
            wr,
            "{}.{}", // TODO(H2CO3): only write 'ctx.' prefix for functions!!!
            NAMING_CONVENTION.context_name,
            name_for_global_func(None /* TODO(H2CO3): namespace */, name, params),
        ),
    }
}

fn write_expr_decl(wr: &mut io::Write, expr: &RcExpr, params: &CodegenParams) -> Result<()> {
    let ptr = expr.borrow()?;
    write_expr_id(wr, &ptr.id, params)?;
    write!(wr, " ")?;
    write_type(wr, &ptr.ty.to_weak(), params)
}

// Helper for writing the prefixed name of a global function. `ns` is the namespace.
fn name_for_global_func(ns: Option<&str>, raw_name: &str, params: &CodegenParams) -> String {
    let name = ns.map_or(String::new(), str::to_owned) + "_" + raw_name;
    transform_func_name(&name, params)
}
