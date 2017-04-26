//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::slice;
use lexer::Token;
use ast::{ Node, NodeValue };


#[allow(missing_debug_implementations)]
struct Parser<'a> {
    tokens: slice::Iter<'a, Token<'a>>,
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

fn error_result<'a>(begin: &'a Token, end: &'a Token, message: String) -> ParseResult<'a> {
    Err(
        ParseError {
            begin:   begin,
            end:     end,
            message: message,
        }
    )
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser { tokens: tokens.iter() }
    }

    fn has_tokens(&self) -> bool {
        self.tokens.len() > 0
    }

    fn parse(mut self) -> ParseResult<'a> {
        let mut children = vec![];
        let first_token = self.tokens.as_slice().first();
        let last_token = self.tokens.as_slice().last();

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
        let token = self.tokens.as_slice().first().expect(
            "parse_toplevel() called with empty input"
        );
        match token.value {
            "struct" => self.parse_struct(),
            "class"  => self.parse_class(),
            "enum"   => self.parse_enum(),
            "fn"     => self.parse_function(),
            _        => error_result(
                token,
                token,
                format!("expected struct, class, enum or function declaration; found '{}'", token.value)
            ),
        }
    }

    fn parse_struct(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_class(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_enum(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_function(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }
}
