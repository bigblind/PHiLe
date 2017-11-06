//
// lexer.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

//! This module contains type definitions and functions for
//! breaking up unstructured source text into lexemes and tokens.
//! It also provides types for mapping tokens to their original
//! location within the source code in a human-readable manner.

use regex::Regex;
use error::{ Error, Result };
use util::{ Ranged, Range, Location, grapheme_count, grapheme_count_by };


/// Given an array of source strings, returns an array of tokens
/// extracted from those strings, or an error if there is a syntactic
/// (more precisely, lexical) error in any of the source strings.
///
/// # Arguments
///
/// * `sources`: a slice of `str`-convertible source strings.
///
/// # Return value
///
/// * `Ok(Vec<Token>)` if the source files were lexically correct.
/// * `Err(Error::Syntax)` if there was a lexical error in the input.
pub fn lex<S: AsRef<str>>(sources: &[S]) -> Result<Vec<Token>> {
    Lexer::new().lex(sources)
}

/// Describes the type of a single token or lexeme.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TokenKind {
    /// Horizontal and vertical (e.g. newline) whitespace. Unicode-aware.
    Whitespace,
    /// Currently, a line comment, beginning with '#' and ending with vertical whitespace or end-of-source.
    Comment,
    /// An identifier or a keyword.
    Word,
    /// Operators and other punctuation characters, e.g. semicolons.
    Punctuation,
    /// String literal.
    String,
    /// Integer or floating-point literal.
    Numeric,
}

/// Represents a lexeme and its associated type and location information as an abstract token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'a> {
    /// The kind associated with the recognized lexeme.
    pub kind: TokenKind,
    /// A pointer into the source where the underlying lexeme was found.
    pub value: &'a str,
    /// Human-readable range information for the underlying lexeme.
    pub range: Range,
}

impl<'a> Ranged for Token<'a> {
    fn range(&self) -> Range {
        self.range
    }
}

trait LocationExt {
    fn advance_by(&self, lexeme: &str) -> Location;
}

impl LocationExt for Location {
    fn advance_by(&self, lexeme: &str) -> Location {
        // TODO(H2CO3): Keep this list in sync with the regexes in Lexer::new()
        let line_breaks: &[char] = &['\n', '\x0b', '\x0c', '\r', '\u{0085}', '\u{2028}', '\u{2029}'];
        match lexeme.rfind(line_breaks) {
            // -1 because the \n itself doesn't count,
            // +1 because humans start counting at 1.
            Some(index) => Location {
                src_idx: self.src_idx,
                line:    self.line + grapheme_count_by(lexeme, |g| g.contains(line_breaks)),
                column:  grapheme_count(&lexeme[index..]) - 1 + 1,
            },
            None => Location {
                src_idx: self.src_idx,
                line:    self.line,
                column:  self.column + grapheme_count(lexeme),
            },
        }
    }
}

const NUM_TOKEN_KINDS: usize = 6;

#[derive(Debug)]
struct Lexer<'a> {
    source: &'a str,
    location: Location,
    tokens: Vec<Token<'a>>,
    regexes: [(TokenKind, Regex); NUM_TOKEN_KINDS],
}

impl<'a> Lexer<'a> {
    fn new() -> Lexer<'a> {
        Lexer {
            source: "",
            location: Default::default(),
            tokens: Vec::new(),
            regexes: [
                (TokenKind::Whitespace,  Regex::new(r#"^\s+"#).unwrap()),
                (TokenKind::Comment,     Regex::new(r#"^#[^\n\v\f\r\x{0085}\x{2028}\x{2029}]*(\r\n|[\n\v\f\r\x{0085}\x{2028}\x{2029}])?"#).unwrap()),
                (TokenKind::Word,        Regex::new(r#"^[_\p{XID_Start}]\p{XID_Continue}*"#).unwrap()),
                (TokenKind::Numeric,     Regex::new(r#"^((0[bB][0-1]+)|(0[oO][0-7]+)|(0[xX][[:xdigit:]]+)|([0-9]+(\.[0-9]+([eE][\+\-]?[0-9]+)?)?))"#).unwrap()),
                (TokenKind::Punctuation, Regex::new(r#"^([!\?\*\+]?<\->[!\?\*\+]?|\.{1,3}|[<>]=?|[=!]=|\->|=>?|::?|[\(\)\[\]\{\}\+\-\*/%&\|~\?,;])"#).unwrap()),
                (TokenKind::String,      Regex::new(r#"^"([^\\"]|\\[\\"nrt]|\\x[[:xdigit:]]{2}|\\u\{[[:xdigit:]]+\})*""#).unwrap()),
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

    #[cfg_attr(feature = "cargo-clippy", allow(should_implement_trait))]
    fn next(&mut self) -> Result<Option<Token<'a>>> {
        if self.source.is_empty() {
            return Ok(None)
        }

        for &(kind, ref re) in &self.regexes {
            if let Some(m) = re.find(self.source) {
                let value = m.as_str();
                let start = self.location;
                let end   = self.location.advance_by(value);
                let range = Range { start, end };
                let token = Token { kind, value, range };

                self.location = end;
                self.source = &self.source[m.end()..];

                return Ok(Some(token));
            }
        }

        Err(Error::Syntax {
            message: "Invalid token".to_owned(),
            range: Range {
                start: self.location,
                end: self.location.advance_by(self.source),
            },
        })
    }
}
