//
// tests/lexer.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 31/07/2017
//

use std::str;
use std::ops;
use std::iter;
use quickcheck::{ Arbitrary, Gen };
use phile::error::Error;
use phile::lexer;


//
// Types for generating correct and incorrect source code
//

// Appends the lexeme onto source's buffer
trait Lexeme: Arbitrary {
    fn render(&self, source: &mut SourceItem);
}

//
// Metadata about a token, without the actual underlying lexeme
//
#[derive(Debug, Clone)]
struct TokenHeader {
    slice: ops::Range<usize>,
    range: lexer::Range,
    kind:  lexer::TokenKind,
}

//
// Represents a multi-file input to the lexer
//
#[derive(Debug, Clone)]
struct GoodSource {
    items: Vec<SourceItem>,
}

impl Arbitrary for GoodSource {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // Generate a random number of source items, but at least 1
        let max_num_sources = 8;
        let num_sources = g.gen_range(1, max_num_sources);
        let mut items: Vec<_> = (0..num_sources).map(SourceItem::new).collect();

        // Fill each item with a random number of correct lexemes, but at least 1
        for item in &mut items {
            let gen_size = 2; // g.size(); // TODO(H2CO3): g.size() once other kinds are implemented
            for _ in 0..g.gen_range(1, gen_size) {
                // TODO(H2CO3): whitespace is not the only kind of correct lexeme
                GoodWhitespace::arbitrary(g).render(&mut *item);
            }
        }

        GoodSource { items }
    }

    // TODO(H2CO3): implement shrink()
}

//
// Represents a single "file" in a multi-source input to the lexer
//
#[derive(Debug, Clone)]
struct SourceItem {
    buf:    String,
    tokens: Vec<TokenHeader>,
    index:  usize,
}

impl SourceItem {
    fn new(index: usize) -> SourceItem {
        SourceItem {
            buf:    String::new(),
            tokens: Vec::new(),
            index:  index,
        }
    }

    fn end_location(&self) -> lexer::Location {
        self.tokens.last().map_or(
            lexer::Location { line: 1, column: 1, src_idx: self.index },
            |token| token.range.end,
        )
    }

    fn push(&mut self, lexeme: &str, kind: lexer::TokenKind, end: lexer::Location) {
        let slice = self.buf.len()..self.buf.len() + lexeme.len();
        let begin = self.end_location();
        let range = lexer::Range { begin, end };
        self.tokens.push(TokenHeader { slice, range, kind });
        self.buf.push_str(lexeme);
    }
}

impl AsRef<str> for SourceItem {
    fn as_ref(&self) -> &str {
        &self.buf
    }
}

//
// A lexeme containing well-formed whitespace characters,
// horizontal as well as vertical.
//
#[derive(Debug, Clone)]
struct GoodWhitespace {
    buf: String,
}

impl Arbitrary for GoodWhitespace {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // generate at least one valid whitespace character
        let buf = (0..g.gen_range(1, 16)).map(|_| {
            let chars = if g.gen() { HOR_WS } else { VER_WS };
            *g.choose(chars).unwrap()
        }).collect();

        GoodWhitespace { buf }
    }

    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        // leave out each character, one-by-one
        let iter = (0..self.buf.chars().count()).map(|i| {
            let buf = self.buf
                .chars()
                .enumerate()
                .filter(|&(j, _)| j != i)
                .map(|(_, c)| c)
                .collect();

            GoodWhitespace { buf }
        });

        Box::new(iter.collect::<Vec<_>>().into_iter())
    }
}

impl Lexeme for GoodWhitespace {
    fn render(&self, source: &mut SourceItem) {
        let mut end = source.end_location();

        for ch in self.buf.chars() {
            if VER_WS.contains(&ch) {
                end.column = 1;
                end.line += 1;
            } else {
                end.column += 1;
            }
        }

        source.push(&self.buf, lexer::TokenKind::Whitespace, end);
    }
}

// Horizontal whitespace
static HOR_WS: &[char] = &[
    '\u{0009}',
    '\u{0020}',
    '\u{00A0}',
    '\u{1680}',
    '\u{2000}',
    '\u{2001}',
    '\u{2002}',
    '\u{2003}',
    '\u{2004}',
    '\u{2005}',
    '\u{2006}',
    '\u{2007}',
    '\u{2008}',
    '\u{2009}',
    '\u{200A}',
    '\u{202F}',
    '\u{205F}',
    '\u{3000}',
];

// Vertical whitespace, a.k.a. newline characters
static VER_WS: &[char] = &[
    '\u{000A}',
    '\u{000B}',
    '\u{000C}',
    '\u{000D}',
    '\u{0085}',
    '\u{2028}',
    '\u{2029}',
];

//
// Actual Unit Tests
//

#[test]
fn no_sources() {
    let sources: &[&str] = &[];
    assert!(lexer::lex(sources).unwrap().is_empty());
}

#[test]
fn single_empty_source() {
    assert!(lexer::lex(&[""]).unwrap().is_empty());
}

#[test]
fn multiple_empty_sources() {
    assert!(lexer::lex(&["", ""]).unwrap().is_empty());
}

#[test]
fn ascii_digit_is_numeric() {
    let lexemes: &[&str] = &[
        "0b0", "0b1", "0B0", "0B1", "0b00", "0b01", "0B00", "0B11",
        "0o0", "0o7", "0O0", "0O6", "0o00", "0o45", "0O32", "0O14",
        "0x0", "0xf", "0X0", "0XD", "0x00", "0x0A", "0X00", "0X0b", "0xC", "0Xf",
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
        "010", "0099", "2468", "13570", "34021",
        "0.0", "00.0", "0.00", "00.00", "1.0", "0.1", "123.468", "10.09",
        "0.0e0", "1.0e0", "0.1e0", "9.9e9", "1.9e5", "00.00e00", "123.456e012",
        "0.0E0", "2.0E0", "0.3E0", "4.5E9", "6.7E5", "00.00E00", "123.456E012",
        "0.0e+0", "6.0e+0", "0.4e+0", "3.6e+9", "1.3e+5", "00.00e+00", "123.456e+012",
        "0.0E+0", "7.0E+0", "0.3E+0", "4.5E+9", "3.5E+5", "00.00E+00", "456.123E+024",
        "0.0e-0", "8.0e-0", "0.2e-0", "5.4e-9", "5.7e-5", "00.00e-00", "246.753e-034",
        "0.0E-0", "9.0E-0", "0.1E-0", "6.3E-9", "7.9E-5", "00.00E-00", "846.195E-029",
        "53.46e24", "39.81E31", "65.43e+21", "75.64E+13", "13.37e-37", "42.24E-31",
    ];

    for lexeme in lexemes {
        // every extended grapheme cluster in this test is ASCII, so it takes
        // up 1 byte => therefore lexeme.len() is OK to use in the Range.
        let range = lexer::Range {
            begin: lexer::Location { line: 1, column: 1, src_idx: 0 },
            end:   lexer::Location { line: 1, column: lexeme.len() + 1, src_idx: 0 },
        };
        let sources = &[lexeme];
        let tokens = lexer::lex(sources).unwrap();
        let token = tokens[0];

        assert_eq!(tokens.len(), 1);
        assert_eq!(token.kind, lexer::TokenKind::NumericLiteral);
        assert_eq!(token.value, *lexeme);
        assert_eq!(token.range, range);
    }
}

#[test]
fn unicode_digit_is_not_numeric() {
    // Characters that are classified as "digit" in Unicode,
    // but should not be accepted by the lexer as such.
    let non_numeric_digits: &[&str] = &[
        "\u{0660}",
        "\u{0967}",
        "\u{0e52}",
        "\u{0967}",
        "\u{1814}",
        "\u{ff15}",
    ];

    // They all express a single grapheme cluster.
    let range = lexer::Range {
        begin: lexer::Location { line: 1, column: 1, src_idx: 0 },
        end:   lexer::Location { line: 1, column: 2, src_idx: 0 },
    };

    for lexeme in non_numeric_digits {
        let sources = &[lexeme];
        let tokens = lexer::lex(sources).unwrap();
        let token = tokens[0];

        assert_eq!(tokens.len(), 1);
        assert_eq!(token.kind, lexer::TokenKind::Word);
        assert_eq!(token.value, *lexeme);
        assert_eq!(token.range, range);
    }
}

quickcheck! {
    #[allow(trivial_casts)]
    fn random_sources(sources: Vec<String>) -> bool {
        match lexer::lex(&sources) {
            Err(Error::Syntax { ref message, range }) => {
                assert_eq!(message, "Invalid token");
                assert!(!sources[range.unwrap().begin.src_idx].is_empty());
                true
            },
            Err(_) => false, // lexer must always produce a syntax error on failure
            Ok(tokens) => sources.iter().all(String::is_empty) || !tokens.is_empty(),
        }
    }

    #[allow(trivial_casts)]
    fn good_source(source: GoodSource) -> bool {
        let actual = lexer::lex(&source.items).unwrap();
        let expected = source.items.iter()
            .flat_map(|item| iter::repeat(item.index).zip(&item.tokens));

        let actual_count = actual.len();
        let expected_count = source.items.iter()
            .flat_map(|item| item.tokens.iter())
            .count();

        assert_eq!(actual_count, expected_count);

        for ((src_idx, exp_tok), act_tok) in expected.zip(actual) {
            assert_eq!(act_tok.kind, exp_tok.kind);
            assert_eq!(act_tok.value, &source.items[src_idx].buf[exp_tok.slice.clone()]);
            assert_eq!(act_tok.range, exp_tok.range);
        }

        true
    }
}
