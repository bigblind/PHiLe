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

    for arg in &func.args {
        let expr = arg.borrow()?;
        match expr.value {
            Value::FuncArg { ref name, index, .. } => {
                if index > 0 {
                    write!(wr, ", ")?
                }

                // TODO(H2CO3): do we need to transform the argument name?
                write!(wr, "{} ", name)?;
                write_type(wr, &expr.ty.as_weak(), params)?;
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
