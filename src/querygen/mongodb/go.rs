//
// querygen/mongodb/go.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 09/07/2017
//

use std::io;
use codegen::*;
use sqir::*;


pub fn generate(sqir: &SQIR, params: &CodegenParams, wp: &mut WriterProvider) -> io::Result<()> {
    for (ns_name, namespace) in &sqir.globals {
        for (raw_name, expr) in namespace {
            let namespace_or = |default| ns_name.as_ref().map_or(default, String::as_ref);
            let file_name = namespace_or("PHiLe-Query").to_owned() + ".go";
            let prefixed_name = namespace_or("").to_owned() + "_" + raw_name;
            let name = transform_func_name(&prefixed_name, params);
            let wptr = wp(&file_name)?;
            let mut wr = wptr.borrow_mut();

            write_global(&mut *wr, &name, &*expr.borrow()?)?;
        }
    }

    Ok(())
}

fn write_global(wr: &mut io::Write, name: &str, expr: &Expr) -> io::Result<()> {
    let typtr = expr.ty.borrow()?;

    let ty = match *typtr {
        Type::Function(ref ty) => ty,
        _ => unreachable!("Non-Function global?!"),
    };

    let func = match expr.value {
        Value::Function(ref func) => func,
        _ => unreachable!("Non-Function global?!"),
    };

    write_function(wr, name, ty, func)
}

fn write_function(_wr: &mut io::Write, _name: &str, _ty: &FunctionType, _func: &Function) -> io::Result<()> {
    unimplemented!()
}
