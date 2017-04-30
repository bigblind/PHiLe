//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use itertools::join;
use std::slice;
use lexer::{ Token, TokenKind, Range };
use ast::*;


struct Parser<'a> {
    tokens: slice::Iter<'a, Token<'a>>,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub range:   Option<Range>,
}

pub type SyntaxResult<T> = Result<T, ParseError>;
pub type ParseResult<'a> = SyntaxResult<Node<'a>>;

type LexResult<'a> = SyntaxResult<&'a Token<'a>>;


pub fn parse<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    Parser::new(tokens).parse()
}

fn token_range(first: &Token, last: &Token) -> Option<Range> {
    Some(
        Range {
            begin: first.range.begin,
            end:   last.range.end,
        }
    )
}

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

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Parser<'a> {
        Parser { tokens: tokens.iter() }
    }

    // Lexer helpers

    fn has_tokens(&self) -> bool {
        self.tokens.len() > 0
    }

    fn expectation_error(&self, expected: &str) -> ParseError {
        let token = self.next_token();
        let actual = token.map_or("end of input", |t| t.value);

        ParseError {
            message: format!("expected '{}'; found '{}'", expected, actual),
            range:   token.map(|t| t.range),
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

        self.next_token().and_then(
            |token| if pred(token) { self.advance() } else { None }
        )
    }

    fn accept(&mut self, lexeme: &str) -> Option<&'a Token<'a>> {
        self.accept_by(|token| token.value == lexeme)
    }

    fn accept_one_of(&mut self, lexemes: &[&str]) -> Option<&'a Token<'a>> {
        self.accept_by(|token| lexemes.contains(&token.value))
    }

    fn expect(&mut self, lexeme: &str) -> LexResult<'a> {
        self.accept(lexeme).ok_or_else(
            || self.expectation_error(lexeme)
        )
    }

    fn expect_one_of(&mut self, lexemes: &[&str]) -> LexResult<'a> {
        self.accept_one_of(lexemes).ok_or_else(
            || self.expectation_error(&join(lexemes, ", "))
        )
    }

    fn accept_identifier(&mut self) -> Option<&'a Token<'a>> {
        self.accept_by(
            |token| token.kind == TokenKind::Word && !is_keyword(token.value)
        )
    }

    fn expect_identifier(&mut self) -> LexResult<'a> {
        self.accept_identifier().ok_or_else(
            || self.expectation_error("identifier")
        )
    }

    fn is_at(&self, lexeme: &str) -> bool {
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

        let range = match (first_token, last_token) {
            (Some(first), Some(last)) => token_range(first, last),
            (_, _)                    => None,
        };
        let node = Node {
            range: range,
            value: NodeValue::Program(children),
        };

        Ok(node)
    }

    fn parse_toplevel(&mut self) -> ParseResult<'a> {
        let error = self.expectation_error("struct, class, enum or fn");
        let token = try!(self.next_token().ok_or_else(|| error.clone()));

        match token.value {
            "struct" | "class" => self.parse_struct_or_class(),
            "enum"             => self.parse_enum(),
            "fn"               => self.parse_function(),
            _                  => Err(error),
        }
    }

    fn parse_struct_or_class(&mut self) -> ParseResult<'a> {
        let mut fields = vec![];

        let keyword = try!(self.expect_one_of(&["struct", "class"]));
        let name = try!(self.expect_identifier());

        try!(self.expect("{"));

        while self.has_tokens() && !self.is_at("}") {
            fields.push(try!(self.parse_field()));
        }

        let close_brace = try!(self.expect("}"));

        let value = match keyword.value {
            "struct" => NodeValue::StructDecl(
                StructDecl {
                    name:   name.value,
                    fields: fields,
                }
            ),
            "class" => NodeValue::ClassDecl(
                ClassDecl {
                    name:   name.value,
                    fields: fields,
                }
            ),
            lexeme => unreachable!("forgot to handle {}", lexeme),
        };
        let node = Node {
            range: token_range(keyword, close_brace),
            value: value,
        };

        Ok(node)
    }

    fn parse_field(&mut self) -> ParseResult<'a> {
        let name = try!(self.expect_identifier());
        let type_decl = try!(self.maybe_parse_type_annotation());
        let relation = try!(self.maybe_parse_relation());
        let comma = try!(self.expect(","));

        let field = Field {
            name:      name.value,
            type_decl: type_decl,
            relation:  relation,
        };
        let node = Node {
            range: token_range(name, comma),
            value: NodeValue::Field(Box::new(field)),
        };

        Ok(node)
    }

    fn maybe_parse_type_annotation(&mut self) -> SyntaxResult<Option<Node<'a>>> {
        let type_decl = match self.accept(":") {
            Some(_) => Some(try!(self.parse_type())),
            None    => None,
        };

        Ok(type_decl)
    }

    fn maybe_parse_relation(&mut self) -> SyntaxResult<Option<Relation<'a>>> {
        #[allow(non_upper_case_globals)]
        static relation_operators: &'static [&'static str] = &[
            // I just couldn't make up my mind as to how to denote
            // "exactly one": by nothing or by an exclamation mark
             "<->",   "<->?",  "<->*",  "<->+",
            "?<->",  "?<->?", "?<->*", "?<->+",
            "*<->",  "*<->?", "*<->*", "*<->+",
            "+<->",  "+<->?", "+<->*", "+<->+",

            "!<->!", "!<->?", "!<->*", "!<->+",
            "?<->!",
            "*<->!",
            "+<->!",
        ];

        let relation = match self.accept_one_of(relation_operators) {
            Some(op) => {
                let entity = try!(self.expect_identifier());
                try!(self.expect("::"));
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
        let mut variants = vec![];

        let enum_keyword = try!(self.expect("enum"));
        let enum_name = try!(self.expect_identifier());

        try!(self.expect("{"));

        while self.has_tokens() && !self.is_at("}") {
            variants.push(try!(self.parse_variant()));
        }

        let close_brace = try!(self.expect("}"));

        let decl = EnumDecl {
            name:     enum_name.value,
            variants: variants,
        };
        let node = Node {
            range: token_range(enum_keyword, close_brace),
            value: NodeValue::EnumDecl(decl),
        };

        Ok(node)
    }

    fn parse_variant(&mut self) -> ParseResult<'a> {
        let mut value_types = vec![];

        let name = try!(self.expect_identifier());

        if self.accept("(").is_some() {
            while self.has_tokens() && !self.is_at(")") {
                value_types.push(try!(self.parse_type()));

                if !self.is_at(")") {
                    try!(self.expect(","));
                }
            }

            try!(self.expect(")"));
        }

        let comma = try!(self.expect(","));

        let variant = Variant {
            name:        name.value,
            value_types: value_types,
        };
        let node = Node {
            range: token_range(name, comma),
            value: NodeValue::Variant(Box::new(variant)),
        };

        Ok(node)
    }

    fn parse_function(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_type(&mut self) -> ParseResult<'a> {
        self.parse_postfix_type()
    }

    fn parse_postfix_type(&mut self) -> ParseResult<'a> {
        let mut node = try!(self.parse_prefix_type());

        loop {
            match self.accept_one_of(&["?", "!"]) {
                Some(token) => {
                    let range = node.range.map(
                        |r| Range { end: token.range.end, .. r }
                    );
                    let value = match token.value {
                        "?" => NodeValue::OptionalType(Box::new(node)),
                        "!" => NodeValue::UniqueType(Box::new(node)),
                        op  => unreachable!("forgot to handle '{}'", op),
                    };
                    node = Node {
                        range: range,
                        value: value,
                    };
                },
                None => break,
            }
        }

        Ok(node)
    }

    fn parse_prefix_type(&mut self) -> ParseResult<'a> {
        // currently, & is the only prefix type operator
        match self.accept("&") {
            Some(token) => {
                let child = try!(self.parse_prefix_type());
                let range = Range {
                    begin: token.range.begin,
                    end:   child.range.map_or(token.range.end, |r| r.end),
                };
                let node = Node {
                    range: Some(range),
                    value: NodeValue::PointerType(Box::new(child)),
                };
                Ok(node)
            },
            None => self.parse_term_type(),
        }
    }

    fn parse_term_type(&mut self) -> ParseResult<'a> {
        match self.next_token().map(|t| t.value) {
            Some("(") => self.parse_tuple_type(),
            Some("[") => self.parse_array_type(),
            Some(_)   => self.parse_named_type(),
            None      => Err(self.expectation_error("parenthesized, named, or array type")),
        }
    }

    fn parse_tuple_type(&mut self) -> ParseResult<'a> {
        let mut items = vec![];
        let open_paren = try!(self.expect("("));

        while self.has_tokens() && !self.is_at(")") {
            items.push(try!(self.parse_type()));

            if !self.is_at(")") {
                try!(self.expect(","));
            }
        }

        let close_paren = try!(self.expect(")"));

        let node = Node {
            range: token_range(open_paren, close_paren),
            value: NodeValue::TupleType(items),
        };

        Ok(node)
    }

    fn parse_array_type(&mut self) -> ParseResult<'a> {
        let open_bracket = try!(self.expect("["));
        let element_type = try!(self.parse_type());
        let close_bracket = try!(self.expect("]"));

        let node = Node {
            range: token_range(open_bracket, close_bracket),
            value: NodeValue::ArrayType(Box::new(element_type)),
        };

        Ok(node)
    }

    fn parse_named_type(&mut self) -> ParseResult<'a> {
        let token = try!(self.expect_identifier());

        let node = Node {
            range: Some(token.range),
            value: NodeValue::NamedType(token.value),
        };

        Ok(node)
    }
}
