//
// tests/lexer.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 31/07/2017
//

#![cfg(test)]
#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications)]

#[macro_use]
extern crate quickcheck;
#[macro_use]
extern crate lazy_static;
extern crate unicode_xid;
extern crate regex;
extern crate phile;

use std::str;
use std::vec;
use std::ops;
use std::iter;
use std::char;
use quickcheck::{ Arbitrary, Gen };
use unicode_xid::UnicodeXID;
use regex::Regex;
use phile::error::Error;
use phile::lexer;
use phile::util::grapheme_count;


//
// Types for generating correct and incorrect source code
//

// Append a lexeme onto `source`'s buffer
trait Lexeme: Arbitrary {
    fn render(&self, source: &mut SourceItem);
}

// Metadata about a token, without the actual underlying lexeme
#[derive(Debug, Clone)]
struct TokenHeader {
    slice: ops::Range<usize>,
    range: lexer::Range,
    kind:  lexer::TokenKind,
}

// Represents a single "file" in multi-source input to the lexer
#[derive(Debug, Clone)]
struct SourceItem {
    buf:    String,
    tokens: Vec<TokenHeader>,
    index:  usize,
}

// Represents some well-formed multi-file input to the lexer
#[derive(Debug, Clone)]
struct ValidSource {
    items: Vec<SourceItem>,
}

// Represents some ill-formed multi-file input to the lexer
#[derive(Debug, Clone)]
struct InvalidSource {
    items: Vec<SourceItem>,
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

impl Arbitrary for ValidSource {
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
                // ValidWhitespace::arbitrary(g).render(&mut *item);
                // ValidLineComment::arbitrary(g).render(&mut *item);
                // ValidWord::arbitrary(g).render(&mut *item);
                ValidNumber::arbitrary(g).render(&mut *item);
            }
        }

        ValidSource { items }
    }

    // Shrinking a multi-item source is not as trivial as removing lexemes
    // one-by-one, because there are many rules concerning consecutive lexemes.
    // TODO(H2CO3): implement shrinking
}

//
// A lexeme containing well-formed whitespace characters,
// horizontal as well as vertical.
//
#[derive(Debug, Clone)]
struct ValidWhitespace {
    buf: String,
}

impl Arbitrary for ValidWhitespace {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // generate at least one valid whitespace character
        let buf = (0..g.gen_range(1, 16)).map(|_| {
            let chars = if g.gen() { HOR_WS } else { VER_WS };
            *g.choose(chars).unwrap()
        }).collect();

        ValidWhitespace { buf }
    }

    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        let num_chars = self.buf.chars().count();

        // do not return empty string as valid whitespace
        if num_chars <= 1 {
            return quickcheck::empty_shrinker()
        }

        // leave out each character, one-by-one
        let iter = (0..num_chars).map(|i| {
            let buf = self.buf
                .chars()
                .enumerate()
                .filter(|&(j, _)| j != i)
                .map(|(_, c)| c)
                .collect();

            ValidWhitespace { buf }
        });

        Box::new(iter.collect::<Vec<_>>().into_iter())
    }
}

impl Lexeme for ValidWhitespace {
    fn render(&self, source: &mut SourceItem) {
        let mut end = source.end_location();

        // A WS char is a single grapheme cluster on its own,
        // so bumping end.column for each char is OK.
        for ch in self.buf.chars() {
            if is_ver_ws(ch) {
                end.column = 1;
                end.line += 1;
            } else {
                end.column += 1;
            }
        }

        source.push(&self.buf, lexer::TokenKind::Whitespace, end);
    }
}

//
// A lexeme representing a line comment ending with a newline.
//
#[derive(Debug, Clone)]
struct ValidLineComment {
    buf: String,
}

impl Arbitrary for ValidLineComment {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // Generate 0 or more valid non-newline extended grapheme clusters.
        // Generating the newline character has to be done first because the
        // map iterator mutably borrows the PRNG for the rest of the fn body.
        let newline = *g.choose(VER_WS).unwrap();
        let it = (0..g.gen_range(0, 16)).map(|_| {
            loop {
                match char::from_u32(g.gen_range(0, char::MAX as u32 + 1)) {
                    Some(ch) => if !is_ver_ws(ch) { return ch },
                    None => {},
                }
            }
        });
        let buf = iter::once('#').chain(it).chain(iter::once(newline)).collect();

        ValidLineComment { buf }
    }

    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        // leave out each character, one-by-one,
        // except the leading '#' and the trailing newline
        let it = (1..self.buf.chars().count() - 1).map(|i| {
            let buf: String = self.buf
                .chars()
                .enumerate()
                .filter(|&(j, _)| j != i)
                .map(|(_, c)| c)
                .collect();

            assert!(buf.chars().next().unwrap() == '#');
            assert!(is_ver_ws(buf.chars().last().unwrap()));

            ValidLineComment { buf }
        });

        Box::new(it.collect::<Vec<_>>().into_iter())
    }
}

impl Lexeme for ValidLineComment {
    fn render(&self, source: &mut SourceItem) {
        // A line comment represented by this type always ends in a newline.
        assert!(self.buf.chars().next().unwrap() == '#');
        assert!(is_ver_ws(self.buf.chars().last().unwrap()));

        let mut end = source.end_location();
        end.line += 1;
        end.column = 1;

        source.push(&self.buf, lexer::TokenKind::Comment, end);
    }
}

// TODO(H2CO3): type for valid line comments that DO NOT end with a newline

//
// A lexeme representing words, that is, identifiers and keywords
//
#[derive(Debug, Clone)]
struct ValidWord {
    buf: String,
}

impl Arbitrary for ValidWord {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // Generate exactly one valid XID_Start char,
        // followed by any number of valid XID_Continue ones.
        let first = loop {
            match char::from_u32(g.gen_range(0, char::MAX as u32 + 1)) {
                Some(ch) => if is_ident_start(ch) { break iter::once(ch) },
                None => {},
            }
        };
        let rest = (0..g.gen_range(0, 16)).map(|_| {
            loop {
                match char::from_u32(g.gen_range(0, char::MAX as u32 + 1)) {
                    Some(ch) => if is_ident_cont(ch) { break ch },
                    None => {},
                }
            }
        });

        ValidWord { buf: first.chain(rest).collect() }
    }

    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        // The first char always has to be a XID_Start, so don't touch it.
        // Leave out the rest of the chars, one by one.
        let it = (1..self.buf.chars().count()).map(|i| {
            let buf: String = self.buf
                .chars()
                .enumerate()
                .filter(|&(j, _)| j != i)
                .map(|(_, c)| c)
                .collect();

            assert!(is_ident_start(buf.chars().next().unwrap()));

            ValidWord { buf }
        });

        Box::new(it.collect::<Vec<_>>().into_iter())
    }
}

impl Lexeme for ValidWord {
    fn render(&self, source: &mut SourceItem) {
        // Identifiers must not contain vertical whitespace
        assert!(!self.buf.contains(VER_WS));
        // ...or any other whitespace, for that matter.
        assert!(!self.buf.contains(HOR_WS));

        let mut end = source.end_location();
        end.column += grapheme_count(&self.buf); // because we contain no newlines

        source.push(&self.buf, lexer::TokenKind::Word, end);
    }
}

//
// A type representing valid numeric (integer and floating-point) literals
//
#[derive(Debug, Clone)]
struct ValidNumber {
    buf: String,
    kind: ValidNumberKind,
    prefix_len: usize, // this many leading bytes to be preserved by shrink()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValidNumberKind {
    DecimalInt,
    BinaryInt,
    OctalInt,
    HexInt,
    FloatingPoint,
}

impl ValidNumber {
    fn arbitrary_int<G: Gen>(g: &mut G, prefix: &str, digits: &[char], kind: ValidNumberKind) -> Self {
        let range = 0..g.gen_range(1, 16);
        let digits_iter = range.map(|_| *g.choose(digits).unwrap());
        let buf = prefix.chars().chain(digits_iter).collect();
        let prefix_len = prefix.len();

        ValidNumber { buf, kind, prefix_len }
    }

    fn arbitrary_decimal_int<G: Gen>(g: &mut G) -> Self {
        Self::arbitrary_int(
            g,
            "",
            &['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'],
            ValidNumberKind::DecimalInt,
        )
    }

    fn arbitrary_binary_int<G: Gen>(g: &mut G) -> Self {
        let prefix = if g.gen() { "0b" } else { "0B" };
        Self::arbitrary_int(
            g,
            prefix,
            &['0', '1'],
            ValidNumberKind::BinaryInt,
        )
    }

    fn arbitrary_octal_int<G: Gen>(g: &mut G) -> Self {
        let prefix = if g.gen() { "0o" } else { "0O" };
        Self::arbitrary_int(
            g,
            prefix,
            &['0', '1', '2', '3', '4', '5', '6', '7'],
            ValidNumberKind::OctalInt,
        )
    }

    fn arbitrary_hex_int<G: Gen>(g: &mut G) -> Self {
        let prefix = if g.gen() { "0x" } else { "0X" };
        Self::arbitrary_int(
            g,
            prefix,
            &[
                '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                'a', 'b', 'c', 'd', 'e', 'f',
                'A', 'B', 'C', 'D', 'E', 'F',
            ],
            ValidNumberKind::HexInt,
        )
    }

    fn decimal_digits<G: Gen>(g: &mut G) -> vec::IntoIter<char> {
        let digits = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];

        (0..g.gen_range(1, 16))
            .map(|_| *g.choose(&digits).unwrap())
            .collect::<Vec<_>>()
            .into_iter()
    }

    fn arbitrary_float<G: Gen>(g: &mut G) -> Self {
        let integral = Self::decimal_digits(g);
        let fraction = Self::decimal_digits(g);
        let dot = iter::once('.');
        let exponent: Box<Iterator<Item=char>> = if g.gen() {
            let e = iter::once(if g.gen() { 'e' } else { 'E' });
            let ds = Self::decimal_digits(g);
            let sign: Box<Iterator<Item=char>> = if g.gen() {
                Box::new(iter::once(if g.gen() { '+' } else { '-' }))
            } else {
                Box::new(iter::empty())
            };

            Box::new(e.chain(sign).chain(ds))
        } else {
            Box::new(iter::empty())
        };
        let buf = integral.chain(dot).chain(fraction).chain(exponent).collect();
        let kind = ValidNumberKind::FloatingPoint;
        let prefix_len = 0;

        ValidNumber { buf, kind, prefix_len }
    }

    fn shrink_int(&self) -> Box<Iterator<Item=Self>> {
        assert!(self.kind != ValidNumberKind::FloatingPoint);
        assert!(self.buf.len() > self.prefix_len);

        let prefix = &self.buf[..self.prefix_len];
        let payload = &self.buf[self.prefix_len..];

        // do not return empty string as a valid integer literal
        if payload.chars().count() <= 1 {
            return quickcheck::empty_shrinker()
        }

        // Remove digits one-by-one
        let it = (0..payload.chars().count()).map(|i| {
            let digits = payload
                .chars()
                .enumerate()
                .filter(|&(j, _)| i != j)
                .map(|(_, c)| c);

            ValidNumber {
                buf: prefix.chars().chain(digits).collect(),
                kind: self.kind,
                prefix_len: self.prefix_len,
            }
        });

        Box::new(it.collect::<Vec<_>>().into_iter())
    }
}

// TODO(H2CO3): this is almost the same as ValidWord::render(); refactor
impl Lexeme for ValidNumber {
    fn render(&self, source: &mut SourceItem) {
        // Identifiers must not contain vertical whitespace
        assert!(!self.buf.contains(VER_WS));
        // ...or any other whitespace, for that matter.
        assert!(!self.buf.contains(HOR_WS));

        let mut end = source.end_location();
        end.column += grapheme_count(&self.buf); // because we contain no newlines

        source.push(&self.buf, lexer::TokenKind::NumericLiteral, end);
    }
}

impl Arbitrary for ValidNumber {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let funcs: &[fn(&mut G) -> ValidNumber] = &[
            Self::arbitrary_decimal_int,
            Self::arbitrary_binary_int,
            Self::arbitrary_octal_int,
            Self::arbitrary_hex_int,
            Self::arbitrary_float,
        ];
        let func = g.choose(funcs).unwrap();

        func(g)
    }

    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        use ValidNumberKind::*;

        match self.kind {
            DecimalInt | BinaryInt | OctalInt | HexInt => self.shrink_int(),
            FloatingPoint => quickcheck::empty_shrinker(), // too hard
        }
    }
}

//
// Constants and functions for generating pseudorandom strings
//

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

const CHAR_MAX_BYTES: usize = 4;

fn is_hor_ws(ch: char) -> bool {
    HOR_WS.contains(&ch)
}

fn is_ver_ws(ch: char) -> bool {
    VER_WS.contains(&ch)
}

// Regex uses an outdated (Unicode 7.0) definition of XID_{Start,Continue},
// whereas the UnicodeXID crate conforms to Unicode 9.0. This makes
// unit tests fail for some code points, e.g. U+17828.
// See https://github.com/rust-lang/regex/issues/391.
// TODO(H2CO3): this is both horribly slow, absolutely disgusting,
// and doesn't actually test _anything_, because the same regex that
// is used for lexing is also utilized in generating test cases.
// Consequently, we must rewrite these two functions using UnicodeXID
// once the regex crate is updated to a more recent version of Unicode...
// When that is done, we can do away with the regex and lazy_static
// imports in the test crate, and with the lazy_static dev dependency.
#[allow(non_upper_case_globals)]
fn is_ident_start(ch: char) -> bool {
    // UnicodeXID::is_xid_start(ch) || ch == '_'

    lazy_static! {
        static ref re: Regex = Regex::new(r"^\p{XID_Start}$").unwrap();
    }

    re.is_match(ch.encode_utf8(&mut [0; CHAR_MAX_BYTES])) || ch == '_'
}

#[allow(non_upper_case_globals)]
fn is_ident_cont(ch: char) -> bool {
    // UnicodeXID::is_xid_continue(ch)

    lazy_static! {
        static ref re: Regex = Regex::new(r"^\p{XID_Continue}$").unwrap();
    }

    re.is_match(ch.encode_utf8(&mut [0; CHAR_MAX_BYTES]))
}

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
        "\u{0E52}",
        "\u{0967}",
        "\u{1814}",
        "\u{FF15}",
    ];

    // They all express a single grapheme cluster.
    let err_range = lexer::Range {
        begin: lexer::Location { line: 1, column: 1, src_idx: 0 },
        end:   lexer::Location { line: 1, column: 2, src_idx: 0 },
    };

    for lexeme in non_numeric_digits {
        let sources = &[lexeme];
        let result = lexer::lex(sources);

        match result {
            Err(Error::Syntax { ref message, range }) => {
                assert_eq!(message, "Invalid token");
                assert_eq!(range, Some(err_range));
            },
            Err(other) => panic!("Lexer returned a non-syntax error: {}", other),
            Ok(tokens) => panic!("Lexer unexpectedly accepted {}: {:#?}", lexeme, tokens),
        }
    }
}

#[test]
fn identifier_with_inner_unicode_digit() {
    let identifiers: &[&str] = &[
        "x\u{0660}",                // Latin Letter followed by Digit
        "_\u{0967}",                // Underscore followed by Digit
        "E\u{0300}\u{0e52}",        // Latin letter + Combining Grave Accent + Digit
        "\u{0687}\u{08EA}\u{0967}", // Arabic + Nonspacing Mark followed by Digit
        "\u{4FA1}\u{1814}",         // Chinese followed by Digit
        "\u{0938}\u{A8F1}\u{FF15}", // Devanagari + Combining Avagraha + Digit
    ];

    for ident in identifiers {
        let range = lexer::Range {
            begin: lexer::Location { line: 1, column: 1, src_idx: 0 },
            end:   lexer::Location { line: 1, column: grapheme_count(ident) + 1, src_idx: 0 },
        };
        let sources = &[ident];
        let tokens = lexer::lex(sources).unwrap();
        let token = tokens[0];

        assert_eq!(tokens.len(), 1);
        assert_eq!(token.kind, lexer::TokenKind::Word);
        assert_eq!(token.value, *ident);
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
            Ok(tokens) => {
                sources.iter().all(String::is_empty) && tokens.is_empty()
                ||
                sources.iter().any(|s| !s.is_empty()) && !tokens.is_empty()
            },
        }
    }

    #[allow(trivial_casts)]
    fn valid_source(source: ValidSource) -> bool {
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
