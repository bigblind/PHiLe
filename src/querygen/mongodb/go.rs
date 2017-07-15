//
// querygen/mongodb/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 09/07/2017
//

use std::io;
use typewriter::go::*;
use codegen::*;
use sqir::*;


//
// Top-level (global func + impl) generators
//

pub fn generate(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    // Write header for yet nonexistent file holding top-level funcs
    let toplevel_basename = "PHiLe-Query";

    {
        let file_name = toplevel_basename.to_owned() + ".go";
        let wr = wp(&file_name)?;
        write_header(&mut *wr.borrow_mut(), params)?;
    }

    for (ns_name, namespace) in &sqir.globals {
        for (raw_name, expr) in namespace {
            let namespace_or = |default| ns_name.as_ref().map_or(default, String::as_ref);
            let file_name = namespace_or(toplevel_basename).to_owned() + ".go";
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

    write_function(wr, name, ty, func, params)
}

//
// Expression Generators
//

fn generate_expr(wr: &mut io::Write, expr: &RcExpr) -> io::Result<()> {
    Ok(()) // TODO(H2CO3): implement
}

fn write_function(
    wr:     &mut io::Write,
    name:   &str,
    ty:     &FunctionType,
    func:   &Function,
    params: &CodegenParams,
) -> io::Result<()> {
    write!(wr, "func {}(", name)?;
    write_ctx_name_and_type(wr)?;

    for arg in &func.args {
        assert_arg_consistency(arg);
        write!(wr, ", ")?;
        write_expr_name_and_type(wr, arg, params)?;
    }

    write!(wr, ") ")?;
    write_type(wr, &ty.ret_type, params)?;
    writeln!(wr, " {{")?;
    generate_expr(wr, &func.body)?;
    write!(wr, "    return ")?;
    write_expr_name(wr, &func.body)?;
    writeln!(wr)?;
    writeln!(wr, "}}")?;
    writeln!(wr)
}

fn assert_arg_consistency(expr: &RcExpr) {
    let ptr = expr.borrow().expect("cannot borrow argument");

    match ptr.value {
        Value::FuncArg { .. } => match ptr.id {
            ExprId::Name(_) => (),
            ExprId::Temp(_) => unreachable!("FuncArg has no name?!"),
        },
        _ => unreachable!("Non-FuncArg argument?!"),
    }
}

// The following functions write references (i.e. reads/loads) and
// variable/argument declarations for all three possible kinds of
// expressions: the special "context" (self/this) parameter,
// named-by-the-user variable bindings, and temporary expressions.
// TODO(H2CO3): should these functions transform names according
// to the name transforms specified by the provided CodegenParams?

fn write_ctx_name(wr: &mut io::Write) -> io::Result<()> {
    write!(wr, "{}", NAMING_CONVENTION.context_name)
}

fn write_ctx_name_and_type(wr: &mut io::Write) -> io::Result<()> {
    write_ctx_name(wr)?;
    write!(wr, " *{}", NAMING_CONVENTION.context_type)
}

fn write_expr_name(wr: &mut io::Write, expr: &RcExpr) -> io::Result<()> {
    match expr.borrow()?.id {
        ExprId::Temp(index)    => write!(wr, "{}{}", NAMING_CONVENTION.tmp_prefix, index),
        ExprId::Name(ref name) => write!(wr, "{}{}", NAMING_CONVENTION.var_prefix, name),
    }
}

fn write_expr_name_and_type(wr: &mut io::Write, expr: &RcExpr, params: &CodegenParams) -> io::Result<()> {
    write_expr_name(wr, expr)?;
    write!(wr, " ")?;
    write_type(wr, &expr.borrow()?.ty.as_weak(), params)
}
