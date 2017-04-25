use regex::Regex;
use unicode_segmentation::UnicodeSegmentation;

fn grapheme_count(lexeme: &str) -> usize {
    UnicodeSegmentation::graphemes(lexeme, true).count()
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    line:   usize,
    column: usize,
}

impl Location {
    fn advance_by(&self, lexeme: &str) -> Location {
        match lexeme.rfind('\n') {
            Some(index) => Location {
                line:   self.line + lexeme.matches('\n').count(),
                // -1 because the \n itself doesn't count,
                // +1 because humans start counting at 1.
                column: grapheme_count(&lexeme[index..]) - 1 + 1,
            },
            None => Location {
                line:   self.line,
                column: self.column + grapheme_count(lexeme),
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Range {
    begin: Location,
    end:   Location,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    Whitespace,
    Comment,
    Identifier,
    Punctuation,
    StringLiteral,
    NumericLiteral,
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    kind:  TokenKind,
    value: &'a str,
    range: Range,
}

#[allow(missing_debug_implementations)]
pub struct Lexer<'a> {
    source:   &'a str,
    location: Location,
    regexes:  [(TokenKind, Regex); 6],
}

impl<'a> Lexer<'a> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source,
            location: Location { line: 1, column: 1 },
            regexes: [
                (TokenKind::Whitespace,     Regex::new(r"^\s+").unwrap()),
                (TokenKind::Comment,        Regex::new(r"^#[^\n]*\n?").unwrap()),
                (TokenKind::Identifier,     Regex::new(r"^[\w_][\w\d_]*").unwrap()),
                (TokenKind::NumericLiteral, Regex::new(r"^((0[bB][0-1]+)|(0[oO][0-7]+)|(0[xX][[:xdigit:]]+)|(\d+(\.\d+([eE][+-]?\d+)?)?))").unwrap()),
                (TokenKind::Punctuation,    Regex::new(r"^(\(|\)|\[|\]|\{|\}|[\?\*\+]?<\->[\?\*\+]?|([<>\+\-\*/%&\|\^!=]=?)|&&|\|\||~|\.|,|\?|::?|;)").unwrap()),
                (TokenKind::StringLiteral,  Regex::new(r#"^"([^\\]|\\["'nrtb]|\\x[[:xdigit:]]{2}|\\U[[:xdigit:]]{8})*""#).unwrap()),
            ],
        }
    }

    pub fn lex(&mut self) -> Result<Vec<Token<'a>>, Location> {
        let mut tokens = vec![];

        loop {
            match self.next() {
                Ok(token)  => tokens.push(token),
                Err(true)  => return Err(self.location),
                Err(false) => return Ok(tokens),
            }
        }
    }

    fn next(&mut self) -> Result<Token<'a>, bool> {
        if self.source.is_empty() {
            return Err(false);
        }

        for &(kind, ref re) in self.regexes.iter() {
            if let Some(m) = re.find(self.source) {
                let value = m.as_str();
                let begin = self.location;
                let end   = self.location.advance_by(value);

                let range = Range {
                    begin: begin,
                    end:   end,
                };
                let token = Token {
                    kind:  kind,
                    value: value,
                    range: range,
                };

                self.location = end;
                self.source = &self.source[m.end()..];

                return Ok(token);
            }
        }

        Err(true)
    }
}
