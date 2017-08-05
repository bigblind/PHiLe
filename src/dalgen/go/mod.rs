//
// dalgen/go/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 18/07/2017
//

use std::io;
use error::{ Error, Result };
use codegen::*;
use sqir::*;
use declgen::go::*;

pub mod sqlite3;
pub mod mongodb;
pub mod mariadb;


//
// Naming Conventions for frequently-occurring types and variables
//

#[derive(Debug, Clone, Copy)]
pub struct NamingConvention {
    pub context_type: &'static str,
    pub context_name: &'static str,
    pub tmp_prefix:   &'static str,
    pub var_prefix:   &'static str,
    pub top_basename: &'static str,
}

static NAMING_CONVENTION: NamingConvention = NamingConvention {
    context_type: "PhileCtx",
    context_name: "ctx",
    tmp_prefix:   "tmp_",
    var_prefix:   "var_",
    top_basename: "PHiLe-Context",
};


pub fn generate_pod(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    // First, generate the schema
    match params.database {
        DatabaseEngine::SQLite3 => sqlite3::generate_schema(sqir, params, wp)?,
        DatabaseEngine::MongoDB => mongodb::generate_schema(sqir, params, wp)?,
        DatabaseEngine::MariaDB => mariadb::generate_schema(sqir, params, wp)?,
    }

    // Then, generate queries
    generate_query(sqir, params, wp)
}

pub fn generate_active_record(_sqir: &Sqir, _params: &CodegenParams, _wp: &mut WriterProvider) -> Result<()> {
    unimplemented!()
}

//
// Top-level (global func + impl) generators
//

fn generate_query(sqir: &Sqir, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
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
            generate_function(wr, ns, Some(name), func, ty, params)
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
        Value::Placeholder        => bug!("Placeholder should have been replaced"),
        Value::Nil                => generate_nil_literal(wr, &ptr.id, params),
        Value::BoolConst(b)       => generate_bool_literal(wr, &ptr.id, b, params),
        Value::IntConst(n)        => generate_int_literal(wr, &ptr.id, n, params),
        Value::FloatConst(x)      => generate_float_literal(wr, &ptr.id, x, params),
        Value::StringConst(ref s) => generate_string_literal(wr, &ptr.id, s, params),
        Value::Load(ref expr)     => generate_load(wr, &ptr.id, expr, params),
        Value::OptionalWrap(_)    => unimplemented!(),
        Value::Ignore(ref expr)   => generate_ignore(wr, &ptr.id, expr, params),
        Value::Seq(ref exprs)     => generate_sequence(wr, &ptr.id, exprs, params),
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
    writeln!(wr, " = {}", escape_string_literal(s))?;
    Ok(())
}

fn generate_load(wr: &mut io::Write, id: &ExprId, expr: &WkExpr, params: &CodegenParams) -> Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, &id, params)?;
    write!(wr, " = ")?;
    let rc = expr.as_rc()?;
    write_expr_id(wr, &rc.borrow()?.id, params)?;
    writeln!(wr)?;
    Ok(())
}

fn generate_ignore(wr: &mut io::Write, id: &ExprId, expr: &RcExpr, params: &CodegenParams) -> Result<()> {
    generate_expr(wr, expr, params)?;
    write!(wr, "    ")?;
    write_expr_id(wr, &id, params)?;
    writeln!(wr, " = struct{{}}{{}}")?;
    Ok(())
}

fn generate_sequence(wr: &mut io::Write, id: &ExprId, exprs: &[RcExpr], params: &CodegenParams) -> Result<()> {
    for expr in exprs {
        generate_expr(wr, expr, params)?
    }

    write!(wr, "    ")?;
    write_expr_id(wr, &id, params)?;
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

    if let Some(ref func_name) = name {
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
            Value::FuncArg { .. } => match ptr.id {
                ExprId::Local(_) => {},
                ExprId::Global(ref name) => bug!("FuncArg is global {}?!", name),
                ExprId::Temp(index) => bug!("FuncArg is temporary {}?!", index),
            },
            ref val => bug!("Non-FuncArg argument?! {:#?}", val),
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
        ExprId::Local(ref name)  => write!(wr, "{}{}", NAMING_CONVENTION.var_prefix, name),
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
    write_type(wr, &ptr.ty.as_weak(), params)
}

// Helper for writing the prefixed name of a global function. `ns` is the namespace.
fn name_for_global_func(ns: Option<&str>, raw_name: &str, params: &CodegenParams) -> String {
    let name = ns.map_or(String::new(), str::to_owned) + "_" + raw_name;
    transform_func_name(&name, params)
}
