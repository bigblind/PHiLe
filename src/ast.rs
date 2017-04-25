//
// ast.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use lexer::Token;


#[derive(Debug, Clone, Copy)]
enum NodeValue {
    StructDecl,
    ClassDecl,
    EnumDecl,
    FunctionDecl,
}

#[derive(Debug)]
struct Node<'a> {
    begin: &'a Token<'a>,
    end:   &'a Token<'a>,
    value: NodeValue,
}
