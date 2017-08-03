//
// parser.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::slice;
use lexer::{ Token, TokenKind, Range, Ranged };
use error::{ Error, Result };
use ast::*;
use util::unescape_string_literal;


struct Parser<'a> {
    tokens: slice::Iter<'a, Token<'a>>,
}

pub type ProgResult<'a> = Result<Prog<'a>>;
pub type ItemResult<'a> = Result<Item<'a>>;
pub type ExpResult<'a>  = Result<Exp<'a>>;
pub type TyResult<'a>   = Result<Ty<'a>>;
pub type LexResult<'a>  = Result<&'a Token<'a>>;


pub fn parse<'a>(tokens: &'a [Token]) -> ProgResult<'a> {
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
        "as",
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

    fn expectation_error(&self, expected: &str) -> Error {
        let token = self.next_token();
        let actual = token.map_or("end of input", |t| t.value);

        Error::Syntax {
            message: format!("Expected {}; found {}", expected, actual),
            range:   token.map(Ranged::range),
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

    fn is_at_one_of(&self, lexemes: &[&str]) -> bool {
        match self.next_token() {
            Some(token) => lexemes.contains(&token.value),
            None        => false,
        }
    }

    // Actual parser methods

    fn parse(mut self) -> ProgResult<'a> {
        let mut items = Vec::new();

        while self.has_tokens() {
            items.push(self.parse_toplevel()?);
        }

        Ok(items)
    }

    fn parse_toplevel(&mut self) -> ItemResult<'a> {
        let err_msg = "struct, class, enum, fn or impl";
        let token = self.next_token().ok_or_else(|| self.expectation_error(err_msg))?;

        match token.value {
            "struct" | "class" => self.parse_struct_or_class(),
            "enum"             => self.parse_enum(),
            "fn"               => self.parse_toplevel_function(),
            "impl"             => self.parse_impl(),
            _                  => Err(self.expectation_error(err_msg)),
        }
    }

    //
    // User-defined Type Definitions
    //

    fn parse_struct_or_class(&mut self) -> ItemResult<'a> {
        let mut fields = Vec::new();

        let keyword = self.expect_one_of(&["struct", "class"])?;
        let name = self.expect_identifier()?.value;

        self.expect("{")?;

        while self.has_tokens() && !self.is_at("}") {
            fields.push(self.parse_field()?);
        }

        let close_brace = self.expect("}")?;

        let range = make_range(keyword, close_brace);
        let item = match keyword.value {
            "struct" => Item::StructDecl(
                StructDecl { range, name, fields }
            ),
            "class" => Item::ClassDecl(
                ClassDecl { range, name, fields }
            ),
            lexeme => bug!("Forgot to handle '{}'", lexeme),
        };

        Ok(item)
    }

    fn parse_field(&mut self) -> Result<Field<'a>> {
        let name_tok = self.expect_identifier()?;
        let name = name_tok.value;
        let ty = self.expect(":").and_then(|_| self.parse_type())?;
        let relation = self.maybe_parse_relation()?;
        let comma = self.expect(",")?;
        let range = make_range(name_tok, comma);

        Ok(Field { range, name, ty, relation })
    }

    fn maybe_parse_relation(&mut self) -> Result<Option<RelDecl<'a>>> {
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

    fn parse_enum(&mut self) -> ItemResult<'a> {
        let mut variants = Vec::new();

        let enum_keyword = self.expect("enum")?;
        let name = self.expect_identifier()?.value;

        self.expect("{")?;

        while self.has_tokens() && !self.is_at("}") {
            variants.push(self.parse_variant()?);
        }

        let close_brace = self.expect("}")?;
        let range = make_range(enum_keyword, close_brace);

        Ok(Item::EnumDecl(EnumDecl { range, name, variants }))
    }

    fn parse_variant(&mut self) -> Result<Variant<'a>> {
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

    fn parse_toplevel_function(&mut self) -> ItemResult<'a> {
        self.parse_function().map(Item::FuncDef)
    }

    fn parse_function(&mut self) -> Result<Function<'a>> {
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

        Ok(Function { range, name, arguments, ret_type, body })
    }

    fn parse_decl_arg(&mut self) -> Result<FuncArg<'a>> {
        let name_tok = self.expect_identifier()?;
        let ty = match self.accept(":") {
            Some(_) => Some(self.parse_type()?),
            None    => None,
        };
        let name = name_tok.value;
        let range = make_range(
            name_tok,
            &ty.as_ref().map_or(name_tok.range, Ranged::range)
        );

        Ok(FuncArg { range, name, ty })
    }

    fn parse_impl(&mut self) -> ItemResult<'a> {
        let mut functions = Vec::new();
        let impl_keyword = self.expect("impl")?;
        let name = self.expect_identifier()?.value;

        self.expect("{")?;

        while self.is_at("fn") {
            functions.push(self.parse_function()?);
        }

        let close_brace = self.expect("}")?;
        let range = make_range(impl_keyword, close_brace);

        Ok(Item::Impl(Impl { range, name, functions }))
    }

    //
    // Statements
    //

    fn parse_stmt(&mut self) -> ExpResult<'a> {
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

    fn parse_vardecl(&mut self) -> ExpResult<'a> {
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
        let kind = ExpKind::VarDecl(Box::new(decl));

        Ok(Exp { kind, range })
    }

    fn parse_empty_stmt(&mut self) -> ExpResult<'a> {
        self.expect(";").map(|semi| Exp {
            kind:  ExpKind::Empty,
            range: semi.range,
        })
    }

    fn parse_expr_stmt(&mut self) -> ExpResult<'a> {
        // If the expression ends with a block, i.e., if it is either
        // a block, an if, a match, a for or while loop (?), a lambda/fn
        // with block body, etc. (!!!), then the terminating semicolon
        // is optional. This is already taken care of by parse_stmt().
        // The semi is also optional after the last statement in a block.
        let expr = self.parse_expr()?;

        if self.accept(";").is_some() {
            let range = expr.range; // TODO(H2CO3): include semicolon
            let kind = ExpKind::Semi(Box::new(expr));
            Ok(Exp { kind, range })
        } else if !self.is_at("}") {
            // no trailing semi but not the last statement of block
            Err(self.expectation_error("} or ;"))
        } else {
            Ok(expr)
        }
    }

    //
    // Expressions
    //

    fn parse_expr(&mut self) -> ExpResult<'a> {
        self.parse_cond_expr()
    }

    fn parse_cond_expr(&mut self) -> ExpResult<'a> {
        let condition = self.parse_range_expr()?;

        if self.accept("?").is_none() {
            return Ok(condition)
        }

        let true_val = self.parse_expr()?;
        self.expect(":")?;
        let false_val = self.parse_cond_expr()?;

        let range = make_range(&condition, &false_val);
        let expr = CondExp { condition, true_val, false_val };
        let kind = ExpKind::CondExp(Box::new(expr));

        Ok(Exp { kind, range })
    }

    fn parse_range_expr(&mut self) -> ExpResult<'a> {
        self.parse_binop_noassoc(
            &["..", "..."],
            Self::parse_or_expr
        )
    }

    fn parse_or_expr(&mut self) -> ExpResult<'a> {
        self.parse_binop_leftassoc(
            &["|"],
            Self::parse_and_expr
        )
    }

    fn parse_and_expr(&mut self) -> ExpResult<'a> {
        self.parse_binop_leftassoc(
            &["&"],
            Self::parse_comparison_expr
        )
    }

    fn parse_comparison_expr(&mut self) -> ExpResult<'a> {
        self.parse_binop_noassoc(
            &["==", "!=", "<", ">", "<=", ">="],
            Self::parse_additive_expr
        )
    }

    fn parse_additive_expr(&mut self) -> ExpResult<'a> {
        self.parse_binop_leftassoc(
            &["+", "-"],
            Self::parse_multiplicative_expr
        )
    }

    fn parse_multiplicative_expr(&mut self) -> ExpResult<'a> {
        self.parse_binop_leftassoc(
            &["*", "/", "%"],
            Self::parse_cast_expr
        )
    }

    fn parse_cast_expr(&mut self) -> ExpResult<'a> {
        let mut expr = self.parse_prefix_expr()?;

        while self.accept("as").is_some() {
            let ty = self.parse_type()?;
            let range = make_range(&expr, &ty);
            let kind = ExpKind::Cast(Box::new(expr), ty);

            expr = Exp { kind, range };
        }

        Ok(expr)
    }

    fn parse_prefix_expr(&mut self) -> ExpResult<'a> {
        let node_ctors: &[&Fn(Box<Exp<'a>>) -> ExpKind] = &[
            &ExpKind::UnaryPlus,
            &ExpKind::UnaryMinus,
            &ExpKind::LogicNot,
        ];
        self.parse_prefix(
            &["+", "-", "~"],
            node_ctors,
            Self::parse_postfix_expr
        )
    }

    fn parse_postfix_expr(&mut self) -> ExpResult<'a> {
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

    fn parse_member_access_expr(&mut self, node: Exp<'a>) -> ExpResult<'a> {
        let op = self.expect_one_of(&[".", "::"])?.value;
        let member_tok = self.expect_identifier()?;
        let base = Box::new(node);
        let member = member_tok.value;
        let range = make_range(&*base, member_tok);
        let kind = match op {
            "."  => ExpKind::MemberAccess(MemberAccess { base, member }),
            "::" => ExpKind::QualAccess(QualAccess { base, member }),
            _    => bug!("forgot to handle '{}'", op),
        };

        Ok(Exp { kind, range })
    }

    fn parse_subscript_expr(&mut self, base: Exp<'a>) -> ExpResult<'a> {
        self.expect("[")?;

        // TODO(H2CO3): parse one or more comma-separated index exprs?
        let index = self.parse_expr()?;
        let close_bracket = self.expect("]")?;
        let range = make_range(&base, close_bracket);
        let expr = Subscript { base, index };
        let kind = ExpKind::Subscript(Box::new(expr));

        Ok(Exp { kind, range })
    }

    fn parse_func_call_expr(&mut self, callee: Exp<'a>) -> ExpResult<'a> {
        let (arguments, arg_range) = self.parse_paren_delim(
            "(", Self::parse_expr, ",", ")"
        )?;
        let range = make_range(&callee, &arg_range);
        let function = Box::new(callee);
        let kind = ExpKind::FuncCall(FuncCall { function, arguments });

        Ok(Exp { kind, range })
    }

    // TODO(H2CO3): parse struct literals and closures
    fn parse_term_expr(&mut self) -> ExpResult<'a> {
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

    fn parse_atomic_expr(&mut self) -> ExpResult<'a> {
        if let Some(token) = self.accept_one_of(&["nil", "true", "false"]) {
            let range = token.range;
            let kind = match token.value {
                "nil"   => ExpKind::NilLiteral,
                "true"  => ExpKind::BoolLiteral(true),
                "false" => ExpKind::BoolLiteral(false),
                lexeme  => bug!("forgot to handle '{}'", lexeme),
            };
            return Ok(Exp { kind, range });
        }

        if let Some(token) = self.accept_of_kind(TokenKind::NumericLiteral) {
            return Self::parse_numeric_literal(token);
        }

        if let Some(token) = self.accept_of_kind(TokenKind::StringLiteral) {
            let s = unescape_string_literal(token.value)?;
            let range = token.range;
            let kind = ExpKind::StringLiteral(s);
            return Ok(Exp { kind, range });
        }

        if let Some(token) = self.accept_identifier() {
            let range = token.range;
            let kind = ExpKind::Identifier(token.value);
            return Ok(Exp { kind, range });
        }

        Err(self.expectation_error("literal or identifier"))
    }

    // Helper for parse_atomic_expr
    fn parse_numeric_literal(token: &Token) -> ExpResult<'a> {
        assert!(token.kind == TokenKind::NumericLiteral);

        // If the lexeme contains a decimal point or an exponent,
        // then it's floating-point, otherwise it's an integer.
        let float_chars: &[char] = &['.', 'e', 'E'];

        let kind = if token.value.contains(float_chars) {
            Self::parse_float_literal(token)?
        } else {
            Self::parse_int_literal(token)?
        };

        let range = token.range;

        Ok(Exp { kind, range })
    }

    // Helper for parse_numeric_literal
    fn parse_float_literal(token: &Token) -> Result<ExpKind<'a>> {
        assert!(token.kind == TokenKind::NumericLiteral);

        let lexeme = token.value;

        lexeme.parse()
            .map(ExpKind::FloatLiteral)
            .map_err(|err| Error::Syntax {
                message: format!("Cannot parse float literal '{}': {}", lexeme, err),
                range: Some(token.range),
            })
    }

    // Helper for parse_numeric_literal
    fn parse_int_literal(token: &Token) -> Result<ExpKind<'a>> {
        assert!(token.kind == TokenKind::NumericLiteral);

        let radix_map = [
            ("0b",  2),
            ("0B",  2),
            ("0o",  8),
            ("0O",  8),
            ("0x", 16),
            ("0X", 16),
        ];

        for &(prefix, radix) in &radix_map {
            if token.value.starts_with(prefix) {
                let lexeme = &token.value[prefix.len()..];
                return Self::parse_int_with_radix(lexeme, radix, token.range);
            }
        }

        Self::parse_int_with_radix(token.value, 10, token.range)
    }

    // helper for parse_int_literal
    fn parse_int_with_radix(lexeme: &str, radix: u32, range: Range) -> Result<ExpKind<'a>> {
        u64::from_str_radix(lexeme, radix)
            .map(ExpKind::IntLiteral)
            .map_err(|err| Error::Syntax {
                message: format!("Cannot parse integer literal '{}': {}", lexeme, err),
                range: Some(range),
            })
    }

    fn parse_tuple_expr(&mut self) -> ExpResult<'a> {
        let (exprs, range) = self.parse_paren_delim("(", Self::parse_expr, ",", ")")?;
        let kind = ExpKind::TupleLiteral(exprs);
        Ok(Exp { kind, range })
    }

    fn parse_array_expr(&mut self) -> ExpResult<'a> {
        let (exprs, range) = self.parse_paren_delim("[", Self::parse_expr, ",", "]")?;
        let kind = ExpKind::ArrayLiteral(exprs);
        Ok(Exp { kind, range })
    }

    fn parse_func_expr(&mut self) -> ExpResult<'a> {
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
        let range = make_range(&arg_range, &body);
        let name = None;
        let decl = Function { range, name, arguments, ret_type, body };
        let kind = ExpKind::FuncExp(Box::new(decl));

        Ok(Exp { kind, range })
    }

    // TODO(H2CO3): this is ugly, refactor
    fn parse_if(&mut self) -> ExpResult<'a> {
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
        let kind = ExpKind::If(Box::new(if_expr));

        Ok(Exp { kind, range })
    }

    fn parse_match(&mut self) -> ExpResult<'a> {
        unimplemented!()
    }

    fn parse_block(&mut self) -> ExpResult<'a> {
        let mut items = Vec::new();
        let open_brace = self.expect("{")?;

        while self.has_tokens() && !self.is_at("}") {
            items.push(self.parse_stmt()?);
        }

        let close_brace = self.expect("}")?;
        let range = make_range(open_brace, close_brace);
        let kind = ExpKind::Block(items);

        Ok(Exp { kind, range })
    }

    //
    // Helpers for parsing expressions with binary infix operators
    //

    fn parse_binop_leftassoc<F>(&mut self, tokens: &[&str], subexpr: F) -> ExpResult<'a>
        where F: Fn(&mut Self) -> ExpResult<'a> {

        let mut lhs = subexpr(self)?;

        while let Some(token) = self.accept_one_of(tokens) {
            let rhs = subexpr(self)?;
            let range = make_range(&lhs, &rhs);
            let op = token.value;
            let expr = BinaryOp { op, lhs, rhs };
            let kind = ExpKind::BinaryOp(Box::new(expr));

            lhs = Exp { kind, range };
        }

        Ok(lhs)
    }

    fn parse_binop_noassoc<F>(&mut self, tokens: &[&str], subexpr: F) -> ExpResult<'a>
        where F: Fn(&mut Self) -> ExpResult<'a> {

        let lhs = subexpr(self)?;

        if let Some(token) = self.accept_one_of(tokens) {
            let rhs = subexpr(self)?;
            let op = token.value;

            if self.is_at_one_of(tokens) {
                let message = format!("Binary operator {} is not associative", op);
                let range = self.next_token().map(Ranged::range);

                Err(Error::Syntax { message, range })
            } else {
                let range = make_range(&lhs, &rhs);
                let expr = BinaryOp { op, lhs, rhs };
                let kind = ExpKind::BinaryOp(Box::new(expr));

                Ok(Exp { kind, range })
            }
        } else {
            Ok(lhs)
        }
    }

    //
    // Built-in Type Declarations
    //

    fn parse_type(&mut self) -> TyResult<'a> {
        self.parse_function_type()
    }

    // TODO(H2CO3): should this be non-associative instead of right-associative?
    fn parse_function_type(&mut self) -> TyResult<'a> {
        let lhs = self.parse_postfix_type()?;

        if self.accept("->").is_none() {
            return Ok(lhs)
        }

        let rhs = self.parse_function_type()?;
        let range = make_range(&lhs, &rhs);

        // decompose tuple type if it appears in argument position
        let arg_types = match lhs.kind {
            TyKind::Tuple(items) => items,
            _ => vec![lhs],
        };

        let ret_type = Box::new(rhs);
        let fn_type = FunctionTy { arg_types, ret_type };
        let kind = TyKind::Function(fn_type);

        Ok(Ty { kind, range })
    }

    fn parse_postfix_type(&mut self) -> TyResult<'a> {
        let mut node = self.parse_prefix_type()?;

        // currently, optional is the only postfix type
        while let Some(token) = self.accept("?") {
            let range = make_range(&node, token);
            let kind = TyKind::Optional(Box::new(node));
            node = Ty { kind, range };
        }

        Ok(node)
    }

    fn parse_prefix_type(&mut self) -> TyResult<'a> {
        self.parse_prefix(
            &["&"],
            &[&TyKind::Pointer],
            Self::parse_term_type
        )
    }

    fn parse_term_type(&mut self) -> TyResult<'a> {
        let token = self.next_token().ok_or_else(
            || self.expectation_error("parenthesized, named, or array type")
        )?;

        match token.value {
            "(" => self.parse_tuple_type(),
            "[" => self.parse_array_type(),
            _   => self.parse_named_type(),
        }
    }

    fn parse_tuple_type(&mut self) -> TyResult<'a> {
        let (items, range) = self.parse_paren_delim("(", Self::parse_type, ",", ")")?;
        let kind = TyKind::Tuple(items);

        Ok(Ty { kind, range })
    }

    fn parse_array_type(&mut self) -> TyResult<'a> {
        let open_bracket = self.expect("[")?;
        let element_type = self.parse_type()?;
        let close_bracket = self.expect("]")?;
        let range = make_range(open_bracket, close_bracket);
        let kind = TyKind::Array(Box::new(element_type));

        Ok(Ty { kind, range })
    }

    fn parse_named_type(&mut self) -> TyResult<'a> {
        let token = self.expect_identifier()?;
        let range = token.range;
        let kind = TyKind::Named(token.value);

        Ok(Ty { kind, range })
    }

    //
    // Generic helpers for parsing either expressions or types
    //

    fn parse_prefix<N, S, K>(&mut self, tokens: &[&str], nodes: &[N], subexpr: S) -> Result<Node<K>>
        where N: Fn(Box<Node<K>>) -> K,
              S: FnOnce(&mut Self) -> Result<Node<K>> {

        assert!(tokens.len() == nodes.len());

        match self.accept_one_of(tokens) {
            Some(token) => {
                let child = self.parse_prefix(tokens, nodes, subexpr)?;
                let range = make_range(token, &child);
                let index = tokens.iter().position(|v| *v == token.value).unwrap();
                let kind = nodes[index](Box::new(child));

                Ok(Node { kind, range })
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
    ) -> Result<(Vec<V>, Range)>
        where P: Fn(&mut Self) -> Result<V> {

        let mut items = Vec::new();
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
