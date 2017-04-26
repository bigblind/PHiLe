//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::slice;
use std::fmt::Debug;
use lexer::{ Token, TokenKind };
use ast::{ Node, NodeValue };


#[allow(missing_debug_implementations)]
struct Parser<'a> {
    tokens: slice::Iter<'a, Token<'a>>,
}

#[derive(Debug, Clone)]
pub struct ParseError<'a> {
    pub message: String,
    pub begin:   Option<&'a Token<'a>>,
    pub end:     Option<&'a Token<'a>>,
}

pub type ParseResult<'a> = Result<Node<'a>, ParseError<'a>>;
type LexResult<'a> = Result<&'a Token<'a>, ParseError<'a>>;


pub fn parse<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    Parser::new(tokens).parse()
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser { tokens: tokens.iter() }
    }

    fn has_tokens(&self) -> bool {
        self.tokens.len() > 0
    }

    fn expect_error<T: ?Sized + Debug>(&self, expected: &T) -> ParseError<'a> {
        let token = self.next_token();
        let actual = token.map_or("end of input".to_owned(), |t| format!("{:#?}", t));

        ParseError {
            message: format!("expected {:#?}; found {}", expected, actual),
            begin:   token,
            end:     token,
        }
    }

    fn next_token(&self) -> Option<&'a Token<'a>> {
        self.tokens.as_slice().first()
    }

    fn advance(&mut self) -> Option<&'a Token<'a>> {
        self.tokens.next()
    }

    fn accept_by<P>(&mut self, pred: P) -> Option<&'a Token<'a>>
        where P: FnOnce(&Token) -> bool {

        match self.next_token() {
            Some(token) => if pred(token) { self.advance() } else { None },
            None        => None,
        }
    }

    fn accept(&mut self, kind: TokenKind) -> Option<&'a Token<'a>> {
        self.accept_by(|token| token.kind == kind)
    }

    fn accept_lexeme(&mut self, lexeme: &str) -> Option<&'a Token<'a>> {
        self.accept_by(|token| token.value == lexeme)
    }

    fn expect(&mut self, kind: TokenKind) -> LexResult<'a> {
        self.accept(kind).ok_or(self.expect_error(&kind))
    }

    fn expect_lexeme(&mut self, lexeme: &str) -> LexResult<'a> {
        self.accept_lexeme(lexeme).ok_or(self.expect_error(lexeme))
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
        let error = self.expect_error("struct, class, enum or fn");
        let token = try!(self.next_token().ok_or_else(|| error.clone()));

        match token.value {
            "struct" => self.parse_struct(),
            "class"  => self.parse_class(),
            "enum"   => self.parse_enum(),
            "fn"     => self.parse_function(),
            _        => Err(error),
        }
    }

    fn parse_struct(&mut self) -> ParseResult<'a> {
        // let struct_keyword = try!(self.expect_lexeme("struct"));
        self.expect_lexeme("struct").map(
            |token| Node {
                begin: Some(token),
                end:   Some(token),
                value: NodeValue::StructDecl
            }
        )
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
