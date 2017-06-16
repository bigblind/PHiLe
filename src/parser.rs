//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::slice;
use lexer::{ Token, TokenKind, Range };
use ast::*;
use error::{ ParseError, SyntaxResult };


struct Parser<'a> {
    tokens: slice::Iter<'a, Token<'a>>,
}

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

fn node_range(first: &Node, last: &Node) -> Option<Range> {
    first.range.and_then(
        |b| last.range.map(
            |e| Range { begin: b.begin, end: e.end }
        )
    )
}

fn is_keyword(lexeme: &str) -> bool {
    #[allow(non_upper_case_globals)]
    static keywords: &[&str] = &[
        "struct",
        "class",
        "enum",
        "fn",
        "impl",
        "let",
        "if",
        "else",
        "match",
        "nil",
        "true",
        "false",
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
            message: format!("Expected '{}'; found '{}'", expected, actual),
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
            || self.expectation_error(&lexemes.join(" or "))
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
            children.push(self.parse_toplevel()?);
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
        let error = self.expectation_error("struct, class, enum, fn or impl");
        let token = self.next_token().ok_or_else(|| error.clone())?;

        match token.value {
            "struct" | "class" => self.parse_struct_or_class(),
            "enum"             => self.parse_enum(),
            "fn"               => self.parse_function(),
            "impl"             => self.parse_impl(),
            _                  => Err(error),
        }
    }

    //
    // User-defined Type Definitions
    //

    fn parse_struct_or_class(&mut self) -> ParseResult<'a> {
        let mut fields = vec![];

        let keyword = self.expect_one_of(&["struct", "class"])?;
        let name = self.expect_identifier()?.value;

        self.expect("{")?;

        while self.has_tokens() && !self.is_at("}") {
            fields.push(self.parse_field()?);
        }

        let close_brace = self.expect("}")?;

        let value = match keyword.value {
            "struct" => NodeValue::StructDecl(
                StructDecl { name, fields }
            ),
            "class" => NodeValue::ClassDecl(
                ClassDecl { name, fields }
            ),
            lexeme => unreachable!("Forgot to handle '{}'", lexeme),
        };
        let node = Node {
            range: token_range(keyword, close_brace),
            value: value,
        };

        Ok(node)
    }

    fn parse_field(&mut self) -> ParseResult<'a> {
        let name_tok = self.expect_identifier()?;
        let name = name_tok.value;
        let type_decl = self.expect(":").and_then(|_| self.parse_type())?;
        let relation = self.maybe_parse_relation()?;
        let comma = self.expect(",")?;

        let field = Field { name, type_decl, relation };
        let node = Node {
            range: token_range(name_tok, comma),
            value: NodeValue::Field(Box::new(field)),
        };

        Ok(node)
    }

    fn maybe_parse_relation(&mut self) -> SyntaxResult<Option<RelDecl<'a>>> {
        #[allow(non_upper_case_globals)]
        static relation_operators: &[&str] = &[
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

        let relation = self.accept_one_of(relation_operators).map(|op| {
            let cardinality = op.value;
            let field = self.accept_identifier().map(|t| t.value);
            RelDecl { cardinality, field }
        });

        Ok(relation)
    }

    fn parse_enum(&mut self) -> ParseResult<'a> {
        let mut variants = vec![];

        let enum_keyword = self.expect("enum")?;
        let name = self.expect_identifier()?.value;

        self.expect("{")?;

        while self.has_tokens() && !self.is_at("}") {
            variants.push(self.parse_variant()?);
        }

        let close_brace = self.expect("}")?;

        let decl = EnumDecl { name, variants };
        let node = Node {
            range: token_range(enum_keyword, close_brace),
            value: NodeValue::EnumDecl(decl),
        };

        Ok(node)
    }

    fn parse_variant(&mut self) -> ParseResult<'a> {
        let name_tok = self.expect_identifier()?;
        let name = name_tok.value;

        let type_decl = match self.accept("(") {
            Some(_) => {
                let decl = self.parse_type()?;
                self.expect(")")?;
                Some(decl)
            },
            None => None,
        };

        let comma = self.expect(",")?;

        let variant = Variant { name, type_decl };
        let node = Node {
            range: token_range(name_tok, comma),
            value: NodeValue::Variant(Box::new(variant)),
        };

        Ok(node)
    }

    //
    // Functions and Type Implementations
    //

    fn parse_function(&mut self) -> ParseResult<'a> {
        let fn_keyword = self.expect("fn")?;
        let name = self.expect_identifier()?.value;
        let arguments = self.parse_decl_args()?;
        let return_type = match self.accept("->") {
            Some(_) => Some(self.parse_type()?),
            None    => None,
        };
        let body = self.parse_block()?;
        let range = body.range.map(
            |r| Range { begin: fn_keyword.range.begin, .. r }
        );
        let decl = FuncDecl { name, arguments, return_type, body };
        let value = NodeValue::FuncDecl(Box::new(decl));

        Ok(Node { range, value })
    }

    fn parse_decl_args(&mut self) -> SyntaxResult<Vec<Node<'a>>> {
        let mut args = vec![];

        self.expect("(")?;

        while self.has_tokens() && !self.is_at(")") {
            let name_tok = self.expect_identifier()?;
            self.expect(":")?;
            let type_decl = Box::new(self.parse_type()?);

            let name = name_tok.value;
            let range = Some(name_tok.range); // TODO(H2CO3): this is a lie
            let value = NodeValue::FuncArg(FuncArg { name, type_decl });

            args.push(Node { range, value });

            if !self.is_at(")") {
                self.expect(",")?;
            }
        }

        self.expect(")")?;

        Ok(args)
    }

    fn parse_impl(&mut self) -> ParseResult<'a> {
        let mut functions = vec![];
        let impl_keyword = self.expect("impl")?;
        let name = self.expect_identifier()?.value;

        self.expect("{")?;

        while self.is_at("fn") {
            functions.push(self.parse_function()?);
        }

        let close_brace = self.expect("}")?;
        let range = token_range(impl_keyword, close_brace);
        let value = NodeValue::Impl(Impl { name, functions });

        Ok(Node { range, value })
    }

    //
    // Statements
    //

    fn parse_stmt(&mut self) -> ParseResult<'a> {
        let tok = self.next_token().ok_or_else(
            || self.expectation_error("statement")
        )?;

        // parse "stmt at toplevel, expr elsewhere" constructs too
        // TODO(H2CO3): if we ever introduce loops (loop, while, for),
        // those should probably be added here as well.
        match tok.value {
            "let"   => self.parse_vardecl(),
            "if"    => self.parse_if(),
            "match" => self.parse_match(),
            "{"     => self.parse_block(),
            ";"     => self.parse_empty_stmt(),
            _       => self.parse_expr_stmt(),
        }
    }

    fn parse_vardecl(&mut self) -> ParseResult<'a> {
        let let_keyword = self.expect("let")?;
        let name_tok = self.expect_identifier()?;

        let type_decl = match self.accept(":") {
            Some(_) => Some(self.parse_type()?),
            None    => None,
        };
        let init_expr = match self.accept("=") {
            Some(_) => Some(self.parse_expr()?),
            None    => None,
        };

        let semicolon = self.expect(";")?;
        let name = name_tok.value;
        let range = token_range(let_keyword, semicolon);
        let decl = VarDecl { name, type_decl, init_expr };
        let value = NodeValue::VarDecl(Box::new(decl));

        Ok(Node { range, value })
    }

    fn parse_empty_stmt(&mut self) -> ParseResult<'a> {
        self.expect(";").map(|semi| Node {
            range: Some(semi.range),
            value: NodeValue::EmptyStmt,
        })
    }

    fn parse_expr_stmt(&mut self) -> ParseResult<'a> {
        // If the expression ends with a block, i.e., if it is either
        // a block, an if, a match, a for or while loop (?), a lambda/fn
        // with block body, etc. (!!!), then the terminating semicolon
        // is optional. This is already taken care of by parse_stmt().
        // The semi is also optional after the last statement in a block.
        let expr = self.parse_expr()?;

        let node = if self.accept(";").is_some() {
            let range = expr.range; // TODO(H2CO3): include semicolon
            let value = NodeValue::Semi(Box::new(expr));
            Node { range, value }
        } else if !self.is_at("}") {
            // no trailing semi but not the last statement of block
            return Err(self.expectation_error("} or ;"))
        } else {
            expr
        };

        Ok(node)
    }

    //
    // Expressions
    //

    fn parse_expr(&mut self) -> ParseResult<'a> {
        self.parse_assign_expr()
    }

    fn parse_assign_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_rightassoc(
            &["=", "+=", "-=", "*=", "/=", "%="],
            Self::parse_cond_expr
        )
    }

    fn parse_cond_expr(&mut self) -> ParseResult<'a> {
        let condition = self.parse_logic_or_expr()?;

        if self.accept("?").is_none() {
            return Ok(condition)
        }

        let true_val = self.parse_expr()?;
        self.expect(":")?;
        let false_val = self.parse_cond_expr()?;

        let range = node_range(&condition, &false_val);
        let expr = CondExpr { condition, true_val, false_val };
        let value = NodeValue::CondExpr(Box::new(expr));

        Ok(Node { range, value })
    }

    fn parse_logic_or_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_leftassoc(
            &["||"],
            Self::parse_logic_and_expr
        )
    }

    fn parse_logic_and_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_leftassoc(
            &["&&"],
            Self::parse_comparison_expr
        )
    }

    fn parse_comparison_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_leftassoc(
            &["==", "!=", "<", ">", "<=", ">="],
            Self::parse_additive_expr
        )
    }

    fn parse_additive_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_leftassoc(
            &["+", "-"],
            Self::parse_multiplicative_expr
        )
    }

    fn parse_multiplicative_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_leftassoc(
            &["*", "/", "%"],
            Self::parse_prefix_expr
        )
    }

    fn parse_prefix_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_subscript_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_member_access_expr(&mut self) -> ParseResult<'a> {
        unimplemented!() // foo.bar
    }

    fn parse_qual_access_expr(&mut self) -> ParseResult<'a> {
        unimplemented!() // Foo::Bar
    }

    fn parse_func_call_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_term_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_atomic_expr(&mut self) -> ParseResult<'a> {
        unimplemented!() // literals and single identifiers
    }

    fn parse_tuple_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_array_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_struct_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_func_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    // TODO(H2CO3): this is ugly, refactor
    fn parse_if(&mut self) -> ParseResult<'a> {
        let if_keyword = self.expect("if")?;
        let condition = self.parse_expr()?;
        let then_arm = self.parse_block()?;
        let else_arm = if self.accept("else").is_some() {
            let expr = if self.is_at("if") {
                self.parse_if()?
            } else {
                self.parse_block()?
            };
            Some(expr)
        } else {
            None
        };

        let range = else_arm.as_ref().unwrap_or(&then_arm).range.map(
            |r| Range { begin: if_keyword.range.begin, .. r }
        );
        let if_expr = If { condition, then_arm, else_arm };
        let value = NodeValue::If(Box::new(if_expr));

        Ok(Node { range, value })
    }

    fn parse_match(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_block(&mut self) -> ParseResult<'a> {
        let mut items = vec![];
        let open_brace = self.expect("{")?;

        while self.has_tokens() && !self.is_at("}") {
            items.push(self.parse_stmt()?);
        }

        let close_brace = self.expect("}")?;
        let range = token_range(open_brace, close_brace);
        let value = NodeValue::Block(items);

        Ok(Node { range, value })
    }

    //
    // Helpers for parsing expressions with binary infix operators
    //

    fn parse_binop_leftassoc<F>(&mut self, tokens: &[&str], subexpr: F) -> ParseResult<'a>
        where F: Fn(&mut Self) -> ParseResult<'a> {

        let mut lhs = subexpr(self)?;

        while let Some(tok) = self.accept_one_of(tokens) {
            let rhs = subexpr(self)?;
            let range = node_range(&lhs, &rhs);
            let op = tok.value;
            let expr = BinaryOp { op, lhs, rhs };
            let value = NodeValue::BinaryOp(Box::new(expr));

            lhs = Node { range, value };
        }

        Ok(lhs)
    }

    fn parse_binop_rightassoc<F>(&mut self, tokens: &[&str], subexpr: F) -> ParseResult<'a>
        where F: Fn(&mut Self) -> ParseResult<'a> {

        let lhs = subexpr(self)?;

        if let Some(tok) = self.accept_one_of(tokens) {
            let rhs = self.parse_binop_rightassoc(tokens, subexpr)?;
            let range = node_range(&lhs, &rhs);
            let op = tok.value;
            let expr = BinaryOp { op, lhs, rhs };
            let value = NodeValue::BinaryOp(Box::new(expr));

            Ok(Node { range, value })
        } else {
            Ok(lhs)
        }
    }

    //
    // Built-in Type Declarations
    //

    fn parse_type(&mut self) -> ParseResult<'a> {
        self.parse_postfix_type()
    }

    fn parse_postfix_type(&mut self) -> ParseResult<'a> {
        let mut node = self.parse_prefix_type()?;

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

                    node = Node { range, value };
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
                let child = self.parse_prefix_type()?;
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
        let open_paren = self.expect("(")?;

        while self.has_tokens() && !self.is_at(")") {
            items.push(self.parse_type()?);

            if !self.is_at(")") {
                self.expect(",")?;
            }
        }

        let close_paren = self.expect(")")?;

        let node = Node {
            range: token_range(open_paren, close_paren),
            value: NodeValue::TupleType(items),
        };

        Ok(node)
    }

    fn parse_array_type(&mut self) -> ParseResult<'a> {
        let open_bracket = self.expect("[")?;
        let element_type = self.parse_type()?;
        let close_bracket = self.expect("]")?;

        let node = Node {
            range: token_range(open_bracket, close_bracket),
            value: NodeValue::ArrayType(Box::new(element_type)),
        };

        Ok(node)
    }

    fn parse_named_type(&mut self) -> ParseResult<'a> {
        let token = self.expect_identifier()?;

        let node = Node {
            range: Some(token.range),
            value: NodeValue::NamedType(token.value),
        };

        Ok(node)
    }
}
