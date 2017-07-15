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
        let expr = arg.borrow()?;
        write!(wr, ", ")?;
        write_expr_name_and_type(wr, arg, params)?;

        // TODO(H2CO3): this verbose assert is ugly af
        match expr.value {
            Value::FuncArg { .. } => match expr.id {
                ExprId::Name(_) => (),
                ExprId::Temp(_) => unreachable!("FuncArg has no name?!"),
            },
            _ => unreachable!("Non-FuncArg argument?!"),
        }
    }

    write!(wr, ") ")?;
    write_type(wr, &ty.ret_type, params)?;
    writeln!(wr, " {{")?;
    writeln!(wr, "    panic(\"not implemented\")")?;
    writeln!(wr, "}}")?;
    writeln!(wr)
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
