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
use ast::*;


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

    // Lexer helpers

    fn is_keyword(lexeme: &str) -> bool {
        #[allow(non_upper_case_globals)]
        static keywords: &'static [&'static str] = &[
            "struct",
            "class",
            "enum",
            "fn",
        ];

        keywords.contains(&lexeme)
    }

    fn has_tokens(&self) -> bool {
        self.tokens.len() > 0
    }

    fn expectation_error<T: ?Sized + Debug>(&self, expected: &T) -> ParseError<'a> {
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

    fn accept_lexemes(&mut self, lexemes: &[&str]) -> Option<&'a Token<'a>> {
        self.accept_by(|token| lexemes.contains(&token.value))
    }

    fn expect(&mut self, kind: TokenKind) -> LexResult<'a> {
        self.accept(kind).ok_or(self.expectation_error(&kind))
    }

    fn expect_lexeme(&mut self, lexeme: &str) -> LexResult<'a> {
        self.accept_lexeme(lexeme).ok_or(self.expectation_error(lexeme))
    }

    fn accept_identifier(&mut self) -> Option<&'a Token<'a>> {
        self.accept_by(
            |token| token.kind == TokenKind::Word && !Self::is_keyword(token.value)
        )
    }

    fn expect_identifier(&mut self) -> LexResult<'a> {
        self.accept_identifier().ok_or(self.expectation_error("identifier"))
    }

    fn is_at(&self, kind: TokenKind) -> bool {
        match self.next_token() {
            Some(token) => token.kind == kind,
            None        => false,
        }
    }

    fn is_at_lexeme(&self, lexeme: &str) -> bool {
        match self.next_token() {
            Some(token) => token.value == lexeme,
            None        => false,
        }
    }

    // Actual parser methods

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
        let error = self.expectation_error("struct, class, enum or fn");
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
        let mut fields = vec![];

        let struct_keyword = try!(self.expect_lexeme("struct"));
        let struct_name = try!(self.expect_identifier());

        try!(self.expect_lexeme("{"));

        while self.has_tokens() && !self.is_at_lexeme("}") {
            fields.push(try!(self.parse_field()));
        }

        let close_brace = try!(self.expect_lexeme("}"));

        let decl = StructDecl {
            name:   struct_name.value,
            fields: fields,
        };
        let node = Node {
            begin: Some(struct_keyword),
            end:   Some(close_brace),
            value: NodeValue::StructDecl(decl),
        };

        Ok(node)
    }

    fn parse_class(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_field(&mut self) -> ParseResult<'a> {
        let name = try!(self.expect_identifier());
        let type_decl = try!(self.maybe_parse_type());
        let relation = try!(self.maybe_parse_relation());
        let comma = try!(self.expect_lexeme(","));

        let field = Field {
            name:      name.value,
            type_decl: type_decl,
            relation:  relation,
        };
        let node = Node {
            begin: Some(name),
            end:   Some(comma),
            value: NodeValue::Field(Box::new(field)),
        };

        Ok(node)
    }

    fn maybe_parse_type(&mut self) -> Result<Option<Node<'a>>, ParseError<'a>> {
        let type_decl = match self.accept_lexeme(":") {
            Some(_) => Some(try!(self.parse_type())),
            None    => None,
        };

        Ok(type_decl)
    }

    fn maybe_parse_relation(&mut self) -> Result<Option<Relation<'a>>, ParseError<'a>> {
        #[allow(non_upper_case_globals)]
        static relation_operators: &'static [&'static str] = &[
            // I just couldn't make up my mind as to how to denote
            // "exactly one": by nothing or by an exclamation mark
             "<->",   "<->?",  "<->*",  "<->+",
            "?<->",  "?<->?", "?<->*", "?<->+",
            "*<->",  "*<->?", "*<->*", "*<->+",
            "+<->",  "+<->?", "+<->*", "+<->+",

            "!<->!", "!<->?", "!<->*", "!<->+",
            "?<->!", "?<->?", "?<->*", "?<->+",
            "*<->!", "*<->?", "*<->*", "*<->+",
            "+<->!", "+<->?", "+<->*", "+<->+",
        ];

        let relation = match self.accept_lexemes(relation_operators) {
            Some(op) => {
                let entity = try!(self.expect_identifier());
                try!(self.expect_lexeme("::"));
                let field = try!(self.expect_identifier());
                let relation = Relation {
                    cardinality: op.value,
                    entity:      entity.value,
                    field:       field.value,
                };
                Some(relation)
            },
            None => None,
        };

        Ok(relation)
    }

    fn parse_enum(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_variant(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_function(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_type(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }
}
