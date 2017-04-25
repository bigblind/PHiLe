//
// ast.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use lexer::Range;


enum NodeValue {
    StructDecl,
    ClassDecl,
    EnumDecl,
    FunctionDecl,
}

struct Node {
    range: Range,
    value: NodeValue,
}
