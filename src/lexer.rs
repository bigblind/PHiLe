//
// lexer.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::fmt::{ self, Display, Formatter };
use regex::Regex;
use error::{ Error, Result };
use util::grapheme_count;


#[derive(Debug, Clone, Copy, Default)]
pub struct Location {
    pub line:    usize,
    pub column:  usize,
    pub src_idx: usize, // index of the source `self` points into
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Range {
    pub begin: Location,
    pub end:   Location,
}

pub trait Ranged {
    fn range(&self) -> Range;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Whitespace,
    Comment,
    Word, // identifier or keyword
    Punctuation,
    StringLiteral,
    NumericLiteral,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind:  TokenKind,
    pub value: &'a str,
    pub range: Range,
}

#[allow(missing_debug_implementations)]
struct Lexer<'a> {
    source:   &'a str,
    location: Location,
    tokens:   Vec<Token<'a>>,
    regexes:  [(TokenKind, Regex); 6],
}


pub fn lex<'a, S: AsRef<str>>(sources: &'a [S]) -> Result<Vec<Token<'a>>> {
    Lexer::new().lex(sources)
}

impl Location {
    // TODO(H2CO3): handle all other Unicode line separators
    fn advance_by(&self, lexeme: &str) -> Location {
        match lexeme.rfind('\n') {
            // -1 because the \n itself doesn't count,
            // +1 because humans start counting at 1.
            Some(index) => Location {
                line:    self.line + lexeme.matches('\n').count(),
                column:  grapheme_count(&lexeme[index..]) - 1 + 1,
                src_idx: self.src_idx,
            },
            None => Location {
                line:    self.line,
                column:  self.column + grapheme_count(lexeme),
                src_idx: self.src_idx,
            },
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "line {}, char {}", self.line, self.column)
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}...{}", self.begin, self.end)
    }
}

impl Ranged for Range {
    fn range(&self) -> Range {
        *self
    }
}

impl<'a> Ranged for Token<'a> {
    fn range(&self) -> Range {
        self.range
    }
}

impl<'a> Lexer<'a> {
    fn new() -> Lexer<'a> {
        Lexer {
            source:   "",
            location: Default::default(),
            tokens:   Vec::new(),
            regexes:  [
                (TokenKind::Whitespace,     Regex::new(r"^\s+").unwrap()),
                (TokenKind::Comment,        Regex::new(r"^#[^\n]*\n?").unwrap()),
                (TokenKind::Word,           Regex::new(r"^[\p{Alphabetic}\p{M}\p{Pc}\p{Join_Control}_][\w_]*").unwrap()),
                (TokenKind::NumericLiteral, Regex::new(r"^((0[bB][0-1]+)|(0[oO][0-7]+)|(0[xX][[:xdigit:]]+)|(\d+(\.\d+([eE][\+\-]?\d+)?)?))").unwrap()),
                (TokenKind::Punctuation,    Regex::new(r"^(\(|\)|\[|\]|\{|\}|[!\?\*\+]?<\->[!\?\*\+]?|\->|=>|([<>\+\-\*/%&\|\^!=]=?)|~|\.{1,3}|,|\?|::?|;)").unwrap()),
                (TokenKind::StringLiteral,  Regex::new(r#"^"([^\\"]|\\["'nrtb]|\\x[[:xdigit:]]{2}|\\U[[:xdigit:]]{8})*""#).unwrap()),
            ],
        }
    }

    fn lex<S: AsRef<str>>(mut self, sources: &'a [S]) -> Result<Vec<Token<'a>>> {
        self.tokens.reserve(sources.iter().map(|s| s.as_ref().len()).sum());

        for source in sources {
            self.source = source.as_ref();
            self.location.line = 1;
            self.location.column = 1;
            self.lex_single_source()?;
            self.location.src_idx += 1;
        }

        Ok(self.tokens)
    }

    fn lex_single_source(&mut self) -> Result<()> {
        loop {
            match self.next() {
                Ok(Some(token)) => self.tokens.push(token),
                Ok(None)        => return Ok(()),
                Err(error)      => return Err(error),
            }
        }
    }

    fn next(&mut self) -> Result<Option<Token<'a>>> {
        if self.source.is_empty() {
            return Ok(None)
        }

        for &(kind, ref re) in &self.regexes {
            if let Some(m) = re.find(self.source) {
                let value = m.as_str();
                let begin = self.location;
                let end   = self.location.advance_by(value);
                let range = Range { begin, end };
                let token = Token { kind, value, range };

                self.location = end;
                self.source = &self.source[m.end()..];

                return Ok(Some(token));
            }
        }

        Err(Error::Syntax {
            message: "Invalid token".to_owned(),
            range: Some(Range {
                begin: self.location,
                end:   self.location.advance_by(self.source),
            }),
        })
    }
}
