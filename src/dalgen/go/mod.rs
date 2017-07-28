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


pub fn generate(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    // First, generate the schema
    match params.database {
        DatabaseEngine::SQLite3 => sqlite3::generate_schema(sqir, params, wp)?,
        DatabaseEngine::MongoDB => mongodb::generate_schema(sqir, params, wp)?,
        DatabaseEngine::MariaDB => mariadb::generate_schema(sqir, params, wp)?,
    }

    // Then, generate queries
    generate_query(sqir, params, wp)
}

//
// Top-level (global func + impl) generators
//

fn generate_query(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> Result<()> {
    for (ns_name, namespace) in &sqir.globals {
        for (raw_name, expr) in namespace {
            let namespace_or = |default| ns_name.as_ref().map_or(default, String::as_ref);
            let file_name = namespace_or(NAMING_CONVENTION.top_basename).to_owned() + ".go";
            let prefixed_name = namespace_or("").to_owned() + "_" + raw_name;
            let name = transform_func_name(&prefixed_name, params);
            let wptr = wp(&file_name)?;
            let mut wr = wptr.try_borrow_mut()?;

            write_global(&mut *wr, &name, &*expr.borrow()?, params)?;
        }
    }

    Ok(())
}

fn write_global(wr: &mut io::Write, name: &str, expr: &Expr, params: &CodegenParams) -> Result<()> {
    let ty_ptr = expr.ty.borrow()?;

    let ty = match *ty_ptr {
        Type::Function(ref ty) => ty,
        ref t => bug!("Non-Function global?! {}", t),
    };

    let func = match expr.value {
        Value::Function(ref func) => func,
        ref val => bug!("Non-Function global?! {:#?}", val),
    };

    generate_function(wr, name, ty, func, params)
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
    name:   &str,
    ty:     &FunctionType,
    func:   &Function,
    params: &CodegenParams,
) -> Result<()> {
    write!(
        wr,
        "func ({} {}) {}(",
        NAMING_CONVENTION.context_name,
        NAMING_CONVENTION.context_type,
        name
    )?;

    for arg in &func.args {
        let ptr = arg.borrow()?;
        match ptr.value {
            Value::FuncArg { index, .. } => match ptr.id {
                ExprId::Local(_) => if index > 0 { write!(wr, ", ")? },
                ExprId::Global(ref name) => bug!("FuncArg is global {}?!", name),
                ExprId::Temp(index) => bug!("FuncArg is temporary {}?!", index),
            },
            ref val => bug!("Non-FuncArg argument?! {:#?}", val),
        }

        write_expr_decl(wr, arg, params)?;
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
// TODO(H2CO3): should these functions transform names according
// to the name transforms specified by the provided CodegenParams?

fn write_expr_id(wr: &mut io::Write, id: &ExprId, params: &CodegenParams) -> io::Result<()> {
    match *id {
        ExprId::Temp(index)      => write!(wr, "{}{}", NAMING_CONVENTION.tmp_prefix, index),
        ExprId::Local(ref name)  => write!(wr, "{}{}", NAMING_CONVENTION.var_prefix, name),
        ExprId::Global(ref name) => write!(wr, "{}.{}", NAMING_CONVENTION.context_type, transform_func_name(name, params)),
    }
}

fn write_expr_decl(wr: &mut io::Write, expr: &RcExpr, params: &CodegenParams) -> Result<()> {
    let ptr = expr.borrow()?;
    write_expr_id(wr, &ptr.id, params)?;
    write!(wr, " ")?;
    write_type(wr, &ptr.ty.as_weak(), params)
}
