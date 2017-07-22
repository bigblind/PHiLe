//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::slice;
use lexer::{ Token, TokenKind, Range, Ranged };
use ast::*;
use error::{ SyntaxError, SyntaxResult };
use util::unescape_string_literal;


struct Parser<'a> {
    tokens: slice::Iter<'a, Token<'a>>,
}

pub type ParseResult<'a> = SyntaxResult<Node<'a>>;
type LexResult<'a> = SyntaxResult<&'a Token<'a>>;


pub fn parse<'a>(tokens: &'a [Token]) -> ParseResult<'a> {
    Parser::new(tokens).parse()
}

fn make_range<F: Ranged, L: Ranged>(first: &F, last: &L) -> Range {
    Range {
        begin: first.range().begin,
        end:   last.range().end,
    }
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

    fn expectation_error(&self, expected: &str) -> SyntaxError {
        let token = self.next_token();
        let actual = token.map_or("end of input", |t| t.value);

        SyntaxError {
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

    fn accept_of_kind(&mut self, kind: TokenKind) -> Option<&'a Token<'a>> {
        self.accept_by(|token| token.kind == kind)
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
            (Some(first), Some(last)) => make_range(first, last),
            _ => Range::default(),
        };
        let value = NodeValue::Program(children);

        Ok(Node { range, value })
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

        let range = make_range(keyword, close_brace);
        let value = match keyword.value {
            "struct" => NodeValue::StructDecl(
                StructDecl { name, fields }
            ),
            "class" => NodeValue::ClassDecl(
                ClassDecl { name, fields }
            ),
            lexeme => unreachable!("Forgot to handle '{}'", lexeme),
        };

        Ok(Node { range, value })
    }

    fn parse_field(&mut self) -> SyntaxResult<Field<'a>> {
        let name_tok = self.expect_identifier()?;
        let name = name_tok.value;
        let ty = self.expect(":").and_then(|_| self.parse_type())?;
        let relation = self.maybe_parse_relation()?;
        let comma = self.expect(",")?;
        let range = make_range(name_tok, comma);

        Ok(Field { range, name, ty, relation })
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
        let range = make_range(enum_keyword, close_brace);
        let value = NodeValue::EnumDecl(decl);

        Ok(Node { range, value })
    }

    fn parse_variant(&mut self) -> SyntaxResult<Variant<'a>> {
        let name_tok = self.expect_identifier()?;
        let name = name_tok.value;

        let ty = match self.accept("(") {
            Some(_) => {
                let decl = self.parse_type()?;
                self.expect(")")?;
                Some(decl)
            },
            None => None,
        };

        let comma = self.expect(",")?;
        let range = make_range(name_tok, comma);

        Ok(Variant { range, name, ty })
    }

    //
    // Functions and Type Implementations
    //

    fn parse_function(&mut self) -> ParseResult<'a> {
        let fn_keyword = self.expect("fn")?;
        let name_tok = self.expect_identifier()?;
        let name = Some(name_tok.value);
        let (arguments, _) = self.parse_paren_delim(
            "(", Self::parse_decl_arg, ",", ")"
        )?;
        let ret_type = if self.accept("->").is_some() {
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = self.parse_block()?;
        let range = make_range(fn_keyword, &body);
        let decl = Function { name, arguments, ret_type, body };
        let value = NodeValue::Function(Box::new(decl));

        Ok(Node { range, value })
    }

    fn parse_decl_arg(&mut self) -> SyntaxResult<FuncArg<'a>> {
        let name_tok = self.expect_identifier()?;
        let ty = match self.accept(":") {
            Some(_) => Some(Box::new(self.parse_type()?)),
            None    => None,
        };
        let name = name_tok.value;
        let begin = name_tok.range.begin;
        let end = ty.as_ref().map_or(name_tok.range.end, |decl| decl.range.end);
        let range = Range { begin, end };

        Ok(FuncArg { range, name, ty })
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
        let range = make_range(impl_keyword, close_brace);
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

        let ty = match self.accept(":") {
            Some(_) => Some(self.parse_type()?),
            None    => None,
        };

        let expr = self.expect("=").and_then(|_| self.parse_expr())?;
        let semicolon = self.expect(";")?;
        let name = name_tok.value;
        let range = make_range(let_keyword, semicolon);
        let decl = VarDecl { name, ty, expr };
        let value = NodeValue::VarDecl(Box::new(decl));

        Ok(Node { range, value })
    }

    fn parse_empty_stmt(&mut self) -> ParseResult<'a> {
        self.expect(";").map(|semi| Node {
            range: semi.range,
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

        let range = make_range(&condition, &false_val);
        let expr = CondExpr { condition, true_val, false_val };
        let value = NodeValue::CondExpr(Box::new(expr));

        Ok(Node { range, value })
    }

    fn parse_logic_or_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_leftassoc(
            &["|"],
            Self::parse_logic_and_expr
        )
    }

    fn parse_logic_and_expr(&mut self) -> ParseResult<'a> {
        self.parse_binop_leftassoc(
            &["&"],
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
        let node_ctors: &[&Fn(Box<Node<'a>>) -> NodeValue] = &[
            &NodeValue::UnaryPlus,
            &NodeValue::UnaryMinus,
            &NodeValue::LogicNot,
        ];
        self.parse_prefix(
            &["+", "-", "~"],
            node_ctors,
            Self::parse_postfix_expr
        )
    }

    fn parse_postfix_expr(&mut self) -> ParseResult<'a> {
        let mut node = self.parse_term_expr()?;

        while let Some(token) = self.next_token() {
            node = match token.value {
                "." | "::" => self.parse_member_access_expr(node)?,
                "["        => self.parse_subscript_expr(node)?,
                "("        => self.parse_func_call_expr(node)?,
                _          => break,
            };
        }

        Ok(node)
    }

    fn parse_member_access_expr(&mut self, node: Node<'a>) -> ParseResult<'a> {
        let op = self.expect_one_of(&[".", "::"])?.value;
        let member_tok = self.expect_identifier()?;
        let base = Box::new(node);
        let member = member_tok.value;
        let range = make_range(&*base, member_tok);
        let value = match op {
            "."  => NodeValue::MemberAccess(MemberAccess { base, member }),
            "::" => NodeValue::QualAccess(QualAccess { base, member }),
            _    => unreachable!("forgot to handle '{}'", op),
        };

        Ok(Node { range, value })
    }

    fn parse_subscript_expr(&mut self, base: Node<'a>) -> ParseResult<'a> {
        self.expect("[")?;

        let index = self.parse_expr()?;
        let close_bracket = self.expect("]")?;
        let range = make_range(&base, close_bracket);
        let expr = Subscript { base, index };
        let value = NodeValue::Subscript(Box::new(expr));

        Ok(Node { range, value })
    }

    fn parse_func_call_expr(&mut self, callee: Node<'a>) -> ParseResult<'a> {
        let (arguments, arg_range) = self.parse_paren_delim(
            "(", Self::parse_expr, ",", ")"
        )?;
        let begin = callee.range.begin;
        let end = arg_range.end;
        let range = Range { begin, end };
        let function = Box::new(callee);
        let value = NodeValue::FuncCall(FuncCall { function, arguments });

        Ok(Node { range, value })
    }

    // TODO(H2CO3): parse struct literals and closures
    fn parse_term_expr(&mut self) -> ParseResult<'a> {
        let token = self.next_token().ok_or_else(
            || self.expectation_error("a term")
        )?;

        match token.value {
            "if"    => self.parse_if(),
            "match" => self.parse_match(),
            "("     => self.parse_tuple_expr(),
            "["     => self.parse_array_expr(),
            "{"     => self.parse_block(),
            "|"     => self.parse_func_expr(),
            _       => self.parse_atomic_expr(),
        }
    }

    fn parse_atomic_expr(&mut self) -> ParseResult<'a> {
        if let Some(token) = self.accept_one_of(&["nil", "true", "false"]) {
            let range = token.range;
            let value = match token.value {
                "nil"   => NodeValue::NilLiteral,
                "true"  => NodeValue::BoolLiteral(true),
                "false" => NodeValue::BoolLiteral(false),
                lexeme  => unreachable!("forgot to handle '{}'", lexeme),
            };
            return Ok(Node { range, value });
        }

        // If the lexeme contains a decimal point or an exponent,
        // then it's floating-point, otherwise it's an integer.
        if let Some(token) = self.accept_of_kind(TokenKind::NumericLiteral) {
            let range = token.range;
            let value = if token.value.contains(|c| "eE.".contains(c)) {
                NodeValue::FloatLiteral(token.value.parse()?)
            } else {
                NodeValue::IntLiteral(token.value.parse()?)
            };
            return Ok(Node { range, value });
        }

        if let Some(token) = self.accept_of_kind(TokenKind::StringLiteral) {
            let s = unescape_string_literal(token.value)?;
            let range = token.range;
            let value = NodeValue::StringLiteral(s);
            return Ok(Node { range, value });
        }

        if let Some(token) = self.accept_identifier() {
            let range = token.range;
            let value = NodeValue::Identifier(token.value);
            return Ok(Node { range, value });
        }

        Err(self.expectation_error("literal or identifier"))
    }

    fn parse_tuple_expr(&mut self) -> ParseResult<'a> {
        let (exprs, range) = self.parse_paren_delim("(", Self::parse_expr, ",", ")")?;
        let value = NodeValue::TupleLiteral(exprs);
        Ok(Node { range, value })
    }

    fn parse_array_expr(&mut self) -> ParseResult<'a> {
        let (exprs, range) = self.parse_paren_delim("[", Self::parse_expr, ",", "]")?;
        let value = NodeValue::ArrayLiteral(exprs);
        Ok(Node { range, value })
    }

    fn parse_struct_expr(&mut self) -> ParseResult<'a> {
        unimplemented!()
    }

    fn parse_func_expr(&mut self) -> ParseResult<'a> {
        let (arguments, arg_range) = self.parse_paren_delim(
            "|", Self::parse_decl_arg, ",", "|"
        )?;
        let has_ret_type = self.accept("->").is_some();
        let ret_type = if has_ret_type {
            Some(self.parse_type()?)
        } else {
            None
        };
        let body = if has_ret_type {
            self.parse_block()?
        } else {
            self.parse_expr()?
        };
        let begin = arg_range.begin;
        let end = body.range.end;
        let range = Range { begin, end };
        let name = None;
        let decl = Function { name, arguments, ret_type, body };
        let value = NodeValue::Function(Box::new(decl));

        Ok(Node { range, value })
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

        let range = make_range(
            if_keyword,
            else_arm.as_ref().unwrap_or(&then_arm)
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
        let range = make_range(open_brace, close_brace);
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
            let range = make_range(&lhs, &rhs);
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
            let range = make_range(&lhs, &rhs);
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
        self.parse_function_type()
    }

    // TODO(H2CO3): should this be non-associative instead of right-associative?
    fn parse_function_type(&mut self) -> ParseResult<'a> {
        let lhs = self.parse_postfix_type()?;

        if self.accept("->").is_none() {
            return Ok(lhs)
        }

        let rhs = self.parse_function_type()?;
        let range = make_range(&lhs, &rhs);

        // decompose tuple type if it appears in argument position
        let arg_types = match lhs.value {
            NodeValue::TupleType(items) => items,
            _ => vec![lhs],
        };

        let ret_type = Box::new(rhs);
        let fn_type = FunctionType { arg_types, ret_type };
        let value = NodeValue::FunctionType(fn_type);

        Ok(Node { range, value })
    }

    fn parse_postfix_type(&mut self) -> ParseResult<'a> {
        let mut node = self.parse_prefix_type()?;

        // currently, optional is the only postfix type
        while let Some(token) = self.accept("?") {
            let range = make_range(&node, token);
            let value = NodeValue::OptionalType(Box::new(node));
            node = Node { range, value };
        }

        Ok(node)
    }

    fn parse_prefix_type(&mut self) -> ParseResult<'a> {
        self.parse_prefix(
            &["&"],
            &[&NodeValue::PointerType],
            Self::parse_term_type
        )
    }

    fn parse_term_type(&mut self) -> ParseResult<'a> {
        let token = self.next_token().ok_or_else(
            || self.expectation_error("parenthesized, named, or array type")
        )?;

        match token.value {
            "(" => self.parse_tuple_type(),
            "[" => self.parse_array_type(),
            _   => self.parse_named_type(),
        }
    }

    fn parse_tuple_type(&mut self) -> ParseResult<'a> {
        let (items, range) = self.parse_paren_delim("(", Self::parse_type, ",", ")")?;
        let value = NodeValue::TupleType(items);
        Ok(Node { range, value })
    }

    fn parse_array_type(&mut self) -> ParseResult<'a> {
        let open_bracket = self.expect("[")?;
        let element_type = self.parse_type()?;
        let close_bracket = self.expect("]")?;
        let range = make_range(open_bracket, close_bracket);
        let value = NodeValue::ArrayType(Box::new(element_type));

        Ok(Node { range, value })
    }

    fn parse_named_type(&mut self) -> ParseResult<'a> {
        let token = self.expect_identifier()?;
        let range = token.range;
        let value = NodeValue::NamedType(token.value);

        Ok(Node { range, value })
    }

    //
    // Generic helpers for parsing either expressions or types
    //

    fn parse_prefix<N, S>(&mut self, tokens: &[&str], nodes: &[N], subexpr: S) -> ParseResult<'a>
        where N: Fn(Box<Node<'a>>) -> NodeValue<'a>,
              S: FnOnce(&mut Self) -> ParseResult<'a> {

        assert!(tokens.len() == nodes.len());

        match self.accept_one_of(tokens) {
            Some(token) => {
                let child = self.parse_prefix(tokens, nodes, subexpr)?;
                let range = make_range(token, &child);
                let index = tokens.iter().position(|v| *v == token.value).unwrap();
                let value = nodes[index](Box::new(child));

                Ok(Node { range, value })
            },
            None => subexpr(self),
        }
    }

    fn parse_paren_delim<P, V>(
        &mut self,
        open:    &str,
        subexpr: P,
        delim:   &str,
        close:   &str
    ) -> SyntaxResult<(Vec<V>, Range)>
        where P: Fn(&mut Self) -> SyntaxResult<V> {

        let mut items = vec![];
        let open_tok = self.expect(open)?;

        while self.has_tokens() && !self.is_at(close) {
            items.push(subexpr(self)?);

            if self.accept(delim).is_none() && !self.is_at(close) {
                let err_msg = format!("{} or {}", delim, close);
                return Err(self.expectation_error(&err_msg));
            }
        }

        let close_tok = self.expect(close)?;
        let range = make_range(open_tok, close_tok);

        Ok((items, range))
    }
}
