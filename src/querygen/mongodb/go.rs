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
            let prefixed_name = format!("{}_{}", namespace_or(""), raw_name);
            let name = transform_func_name(&prefixed_name, params);
            let wptr = wp(&file_name)?;
            let mut wr = wptr.borrow_mut();

            match expr.borrow()?.value {
                Value::Function(ref func) => write_function(&mut *wr, &name, func)?,
                _ => unreachable!("Non-Function global?!"),
            }
        }
    }

    Ok(())
}

fn write_function(wr: &mut io::Write, name: &str, func: &Function) -> io::Result<()> {
    unimplemented!()
}
