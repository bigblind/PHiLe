//
// dalgen/mongodb/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 08/06/2017
//

use std::io;
use sqir::*;
use codegen::*;
use declgen::go::*;
use util::PACKAGE_INFO;


pub fn generate(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    generate_schema(sqir, params, wp)?;
    generate_query(sqir, params, wp)?;

    Ok(())
}

//
// Generating the Schema (DDL)
//

fn generate_schema(_sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    let file_name = TOPLEVEL_BASENAME.to_owned() + ".go";
    let wptr = wp(&file_name)?;
    let mut wr = wptr.borrow_mut();
    let package_name = params.namespace.as_ref().map(
        |ns| transform_namespace(ns, params)
    ).ok_or_else(
        || io::Error::new(io::ErrorKind::InvalidInput, "Missing namespace")
    )?;

    write!(
        &mut *wr,
        include_str!("go_template.txt"),
        version = PACKAGE_INFO.version,
        authors = PACKAGE_INFO.authors,
        ctxtype = NAMING_CONVENTION.context_type,
        namespace = package_name,
    )
}

//
// Generating Queries (DML)
//

//
// Top-level (global func + impl) generators
//

fn generate_query(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    for (ns_name, namespace) in &sqir.globals {
        for (raw_name, expr) in namespace {
            let namespace_or = |default| ns_name.as_ref().map_or(default, String::as_ref);
            let file_name = namespace_or(TOPLEVEL_BASENAME).to_owned() + ".go";
            let prefixed_name = namespace_or("").to_owned() + "_" + raw_name;
            let name = transform_func_name(&prefixed_name, params);
            let wptr = wp(&file_name)?;
            let mut wr = wptr.borrow_mut();

            write_global(&mut *wr, &name, &*expr.borrow()?, params)?;
        }
    }

    Ok(())
}

fn write_global(wr: &mut io::Write, name: &str, expr: &Expr, params: &CodegenParams) -> io::Result<()> {
    let typtr = expr.ty.borrow()?;

    let ty = match *typtr {
        Type::Function(ref ty) => ty,
        _ => unreachable!("Non-Function global?!"),
    };

    let func = match expr.value {
        Value::Function(ref func) => func,
        _ => unreachable!("Non-Function global?!"),
    };

    generate_function(wr, name, ty, func, params)
}

//
// Expression Generators
//

fn generate_expr(wr: &mut io::Write, expr: &RcExpr, params: &CodegenParams) -> io::Result<()> {
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
    write_expr_id(wr, &ptr.id)?;
    writeln!(wr)?;

    match ptr.value {
        Value::Placeholder => unreachable!("Placeholder should have been replaced"),
        Value::Nil                => generate_nil_literal(wr, &ptr.id),
        Value::BoolConst(b)       => generate_bool_literal(wr, &ptr.id, b),
        Value::IntConst(n)        => generate_int_literal(wr, &ptr.id, n),
        Value::FloatConst(x)      => generate_float_literal(wr, &ptr.id, x),
        Value::StringConst(ref s) => generate_string_literal(wr, &ptr.id, s),
        Value::Load(ref expr)     => generate_load(wr, &ptr.id, expr),
        Value::OptionalWrap(_)    => unimplemented!(),
        Value::Ignore(ref expr)   => generate_ignore(wr, &ptr.id, expr, params),
        Value::Seq(ref exprs)     => generate_sequence(wr, &ptr.id, exprs, params),
        _ => unimplemented!(),
    }
}

fn generate_nil_literal(wr: &mut io::Write, id: &ExprId) -> io::Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id)?;
    writeln!(wr, " = nil")
}

fn generate_bool_literal(wr: &mut io::Write, id: &ExprId, b: bool) -> io::Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id)?;
    writeln!(wr, " = {}", b)
}

fn generate_int_literal(wr: &mut io::Write, id: &ExprId, n: u64) -> io::Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id)?;
    writeln!(wr, " = {}", n)
}

fn generate_float_literal(wr: &mut io::Write, id: &ExprId, x: f64) -> io::Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id)?;
    writeln!(wr, " = {}", x)
}

fn generate_string_literal(wr: &mut io::Write, id: &ExprId, s: &str) -> io::Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, id)?;
    writeln!(wr, " = {}", escape_string_literal(s))
}

fn generate_load(wr: &mut io::Write, id: &ExprId, expr: &WkExpr) -> io::Result<()> {
    write!(wr, "    ")?;
    write_expr_id(wr, &id)?;
    write!(wr, " = ")?;
    let rc = expr.as_rc()?;
    write_expr_id(wr, &rc.borrow()?.id)?;
    writeln!(wr)
}

fn generate_ignore(wr: &mut io::Write, id: &ExprId, expr: &RcExpr, params: &CodegenParams) -> io::Result<()> {
    generate_expr(wr, expr, params)?;
    write!(wr, "    ")?;
    write_expr_id(wr, &id)?;
    writeln!(wr, " = struct{{}}{{}}")
}

fn generate_sequence(wr: &mut io::Write, id: &ExprId, exprs: &[RcExpr], params: &CodegenParams) -> io::Result<()> {
    for expr in exprs {
        generate_expr(wr, expr, params)?
    }

    write!(wr, "    ")?;
    write_expr_id(wr, &id)?;
    write!(wr, " = ")?;

    match exprs.last() {
        Some(expr) => {
            write_expr_id(wr, &expr.borrow()?.id)?;
            writeln!(wr)
        },
        None => writeln!(wr, "struct{{}}{{}}"),
    }
}

fn generate_function(
    wr:     &mut io::Write,
    name:   &str,
    ty:     &FunctionType,
    func:   &Function,
    params: &CodegenParams,
) -> io::Result<()> {
    write!(
        wr,
        "func ({} *{}) {}(",
        NAMING_CONVENTION.context_name,
        NAMING_CONVENTION.context_type,
        name
    )?;

    for arg in &func.args {
        let ptr = arg.borrow()?;
        match ptr.value {
            Value::FuncArg { index, .. } => match ptr.id {
                ExprId::Name(_) => if index > 0 {
                    write!(wr, ", ")?
                },
                ExprId::Temp(_) => unreachable!("FuncArg has no name?!"),
            },
            _ => unreachable!("Non-FuncArg argument?!"),
        }

        write_expr_decl(wr, arg, params)?;
    }

    write!(wr, ") ")?;
    write_type(wr, &ty.ret_type, params)?;
    writeln!(wr, " {{")?;
    generate_expr(wr, &func.body, params)?;
    write!(wr, "    return ")?;
    write_expr_id(wr, &func.body.borrow()?.id)?;
    writeln!(wr)?;
    writeln!(wr, "}}")?;
    writeln!(wr)
}

// The following functions write references (i.e. reads/loads) and
// variable/argument declarations for named-by-the-user variable
// bindings and temporary expressions.
// TODO(H2CO3): should these functions transform names according
// to the name transforms specified by the provided CodegenParams?

fn write_expr_id(wr: &mut io::Write, id: &ExprId) -> io::Result<()> {
    match *id {
        ExprId::Temp(index)    => write!(wr, "{}{}", NAMING_CONVENTION.tmp_prefix, index),
        ExprId::Name(ref name) => write!(wr, "{}{}", NAMING_CONVENTION.var_prefix, name),
    }
}

fn write_expr_decl(wr: &mut io::Write, expr: &RcExpr, params: &CodegenParams) -> io::Result<()> {
    let ptr = expr.borrow()?;
    write_expr_id(wr, &ptr.id)?;
    write!(wr, " ")?;
    write_type(wr, &ptr.ty.as_weak(), params)
}
