//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use lexer::Token;


#[allow(missing_debug_implementations)]
pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
}

#[derive(Debug)]
pub struct ParserError<'a> {
    pub begin:   &'a Token<'a>,
    pub end:     &'a Token<'a>,
    pub message: String,
}


impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser { tokens: tokens }
    }
}
