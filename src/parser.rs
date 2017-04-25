//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use lexer::Token;
use ast::{ Node, NodeValue };


#[allow(missing_debug_implementations)]
struct Parser<'a> {
    tokens: &'a [Token<'a>],
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub begin:   &'a Token<'a>,
    pub end:     &'a Token<'a>,
    pub message: String,
}

pub type ParseResult<'a> = Result<Node<'a>, ParseError<'a>>;


pub fn parse<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    Parser::new(tokens).parse()
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser { tokens: tokens }
    }

    fn has_tokens(&self) -> bool {
        self.tokens.len() > 0
    }

    fn parse(&mut self) -> ParseResult<'a> {
        let mut children = Vec::new();
        let first_token = self.tokens.first();
        let last_token = self.tokens.last();

        while self.has_tokens() {
            children.push(try!(self.parse_toplevel()));
        }

        let node = Node {
            begin: first_token,
            end:   last_token,
            value: NodeValue::Program(children),
        };

        Ok(node)
    }

    fn parse_toplevel(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }
}
