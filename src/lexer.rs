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
use util::{ grapheme_count, grapheme_count_by };


/// Represents the location of a single extended grapheme cluster
/// in the sources fed to the lexer.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    /// 0-based index of the source that this location points into.
    pub src_idx: usize,
    /// 1-based line index within the aforementioned source.
    pub line:    usize,
    /// 1-based character index within the line.
    pub column:  usize,
}

/// A half-open range representing a source span.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Range {
    /// Location at the beginning of the source range.
    pub begin: Location,
    /// Location one past the end of the source range.
    pub end:   Location,
}

/// This trait is to be implemented by entities that correspond
/// to some range in the source. This is used for generating
/// location information in user-visible error messages.
pub trait Ranged {
    /// Returns the range `self` was generated from.
    fn range(&self) -> Range;
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
    StringLiteral,
    /// Integer or floating-point literal.
    NumericLiteral,
}

/// Represents a lexeme and its associated type and location information as an abstract token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Token<'a> {
    /// The kind associated with the recognized lexeme.
    pub kind:  TokenKind,
    /// A pointer into the source where the underlying lexeme was found.
    pub value: &'a str,
    /// Human-readable range information for the underlying lexeme.
    pub range: Range,
}

#[derive(Debug)]
struct Lexer<'a> {
    source:   &'a str,
    location: Location,
    tokens:   Vec<Token<'a>>,
    regexes:  [(TokenKind, Regex); 6],
}


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
pub fn lex<'a, S: AsRef<str>>(sources: &'a [S]) -> Result<Vec<Token<'a>>> {
    Lexer::new().lex(sources)
}

impl Location {
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
                (TokenKind::Whitespace,     Regex::new(r#"^\s+"#).unwrap()),
                (TokenKind::Comment,        Regex::new(r#"^#[^\n\v\f\r\x{0085}\x{2028}\x{2029}]*(\r\n|[\n\v\f\r\x{0085}\x{2028}\x{2029}])?"#).unwrap()),
                (TokenKind::Word,           Regex::new(r#"^[_\p{XID_Start}]\p{XID_Continue}*"#).unwrap()),
                (TokenKind::NumericLiteral, Regex::new(r#"^((0[bB][0-1]+)|(0[oO][0-7]+)|(0[xX][[:xdigit:]]+)|([0-9]+(\.[0-9]+([eE][\+\-]?[0-9]+)?)?))"#).unwrap()),
                (TokenKind::Punctuation,    Regex::new(r#"^([!\?\*\+]?<\->[!\?\*\+]?|\.{1,3}|[<>]=?|[=!]=|\->|=>?|::?|[\(\)\[\]\{\}\+\-\*/%&\|~\?,;])"#).unwrap()),
                (TokenKind::StringLiteral,  Regex::new(r#"^"([^\\"]|\\[\\"nrt]|\\x[[:xdigit:]]{2}|\\u\{[[:xdigit:]]+\})*""#).unwrap()),
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
