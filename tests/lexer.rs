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
extern crate unicode_segmentation;
extern crate regex;
extern crate rand;
extern crate phile;

use std::str;
use std::vec;
use std::ops;
use std::iter;
use std::char;
use std::ascii::AsciiExt;
use std::fmt::Write;
use quickcheck::{ Arbitrary, Gen };
use unicode_xid::UnicodeXID;
use unicode_segmentation::UnicodeSegmentation;
use regex::Regex;
use phile::error::Error;
use phile::lexer;
use phile::util::grapheme_count;


//
// Types for generating correct and incorrect source code
//

// Append a lexeme onto `source`'s buffer
trait Lexeme: Arbitrary {
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind;
}

// Metadata about a token, without the actual underlying lexeme
#[derive(Debug, Clone)]
struct TokenHeader {
    slice: ops::Range<usize>,
    range: lexer::Range,
    kind:  lexer::TokenKind,
}

// Represents some well-formed multi-file input to the lexer
#[derive(Debug, Clone)]
struct ValidSource {
    files: Vec<ValidSourceFile>,
}

type ValidSourceFile = Vec<ValidToken>;

// Poor man's dynamic dispatch for non-object-safe traits: enums!
#[derive(Debug, Clone)]
enum ValidToken {
    Whitespace(ValidWhitespace),
    LineComment(ValidLineComment),
    Word(ValidWord),
    Number(ValidNumber),
    Punct(ValidPunct),
    String(ValidString),
}


impl ValidSource {
    fn render(&self) -> (Vec<String>, Vec<TokenHeader>) {
        let mut strings = Vec::with_capacity(self.files.len());
        let mut tokens = Vec::with_capacity(self.files.len() * 100);

        for (i, file) in self.files.iter().enumerate() {
            let mut buf = String::with_capacity(file.len() * 16);
            let mut begin = lexer::Location { src_idx: i, line: 1, column: 1 };

            for token in file {
                let mut end = begin;
                let old_len = buf.len();
                let kind = token.render(&mut buf, &mut end);
                let new_len = buf.len();
                let slice = old_len..new_len;
                let range = lexer::Range { begin, end };
                tokens.push(TokenHeader { slice, range, kind });
                begin = end;
            }

            strings.push(buf);
        }

        (strings, tokens)
    }
}

impl Arbitrary for ValidSource {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        // Generate a random number of source "files", but at least 1
        // Fill each file with a random number of correct lexemes, but at least 1
        let files = (0..g.gen_range(1, 8)).map(|_| {
            let gen_size = g.size();

            (0..g.gen_range(1, gen_size)).flat_map(|_| {
                let fill = match g.gen() {
                    false => ValidToken::Whitespace(ValidWhitespace::arbitrary(g)),
                    true  => ValidToken::LineComment(ValidLineComment::arbitrary(g)),
                };

                let non_ws = match g.gen_range(0, 4) {
                    0 => ValidToken::Word(ValidWord::arbitrary(g)),
                    1 => ValidToken::Number(ValidNumber::arbitrary(g)),
                    2 => ValidToken::Punct(ValidPunct::arbitrary(g)),
                    3 => ValidToken::String(ValidString::arbitrary(g)),
                    _ => unreachable!(),
                };

                iter::once(fill).chain(iter::once(non_ws))
            }).collect()
        }).collect();

        ValidSource { files }
    }

    // Shrinking a multi-item source is not as trivial as removing lexemes
    // one-by-one, because there are many rules concerning consecutive lexemes.
    // TODO(H2CO3): implement shrinking
}

impl ValidToken {
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind {
        use ValidToken::*;

        match *self {
            Whitespace(ref ws)  => ws.render(buf, end),
            LineComment(ref lc) => lc.render(buf, end),
            Word(ref word)      => word.render(buf, end),
            Number(ref num)     => num.render(buf, end),
            Punct(ref punct)    => punct.render(buf, end),
            String(ref s)       => s.render(buf, end),
        }
    }
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
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind {
        for g in self.buf.graphemes(true) {
            if g.contains(VER_WS) {
                end.column = 1;
                end.line += 1;
            } else {
                end.column += 1;
            }
        }

        buf.push_str(&self.buf);

        lexer::TokenKind::Whitespace
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
                let ch: char = g.gen();

                if !is_ver_ws(ch) {
                    break ch
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
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind {
        // A line comment represented by this type always ends in a newline.
        assert!(self.buf.chars().next().unwrap() == '#');
        assert!(is_ver_ws(self.buf.chars().last().unwrap()));

        end.line += 1;
        end.column = 1;

        buf.push_str(&self.buf);

        lexer::TokenKind::Comment
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
            let ch: char = g.gen();

            if is_ident_start(ch) {
                break iter::once(ch)
            }
        };
        let rest = (0..g.gen_range(0, 16)).map(|_| {
            loop {
                let ch: char = g.gen();

                if is_ident_cont(ch) {
                    break ch
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
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind {
        // Identifiers must not contain whitespace, including newlines
        assert!(!self.buf.contains(char::is_whitespace));

        end.column += grapheme_count(&self.buf); // because we contain no newlines
        buf.push_str(&self.buf);

        lexer::TokenKind::Word
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
        assert!(self.prefix_len < self.buf.len());

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
                .filter(|&(j, _)| j != i)
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

// TODO(H2CO3): this is almost the same as ValidWord::render(); refactor
impl Lexeme for ValidNumber {
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind {
        // Identifiers must not contain whitespace, including newlines
        assert!(!self.buf.contains(char::is_whitespace));

        end.column += grapheme_count(&self.buf); // because we contain no newlines
        buf.push_str(&self.buf);

        lexer::TokenKind::NumericLiteral
    }
}

//
// Type for generating an operator or other punctuation character
//
#[derive(Debug, Clone)]
struct ValidPunct {
    value: &'static str,
}

// No shrink(); operators are atomic
impl Arbitrary for ValidPunct {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        ValidPunct { value: *g.choose(PUNCTUATION).unwrap() }
    }
}

impl Lexeme for ValidPunct {
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind {
        assert!(!self.value.contains(char::is_whitespace));
        assert!(self.value.is_ascii());

        end.column += grapheme_count(self.value);
        buf.push_str(self.value);

        lexer::TokenKind::Punctuation
    }
}

//
// Type for generating valid string literals
//
#[derive(Debug, Clone)]
struct ValidString {
    chars: Vec<CharacterLiteral>,
}

#[derive(Debug, Clone, Copy)]
enum CharacterLiteral {
    Unescaped(char),
    Quote,
    Backslash,
    Cr,
    Lf,
    Tab,
    Hex(u8),
    Unicode(char),
}

impl Arbitrary for ValidString {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        use CharacterLiteral::*;

        // generate a possibly empty string, 25% of which are escape sequences
        let it = (0..g.gen_range(0, 16)).map(|_| {
            if g.gen::<u8>() < 0x40 {
                return match g.gen::<u8>() {
                    0x00...0x1f => Quote,            // 2/16
                    0x20...0x3f => Backslash,        // 2/16
                    0x40...0x5f => Cr,               // 2/16
                    0x60...0x7f => Lf,               // 2/16
                    0x80...0x9f => Tab,              // 2/16
                    0xa0...0xcf => Hex(g.gen()),     // 3/16
                    0xd0...0xff => Unicode(g.gen()), // 3/16
                    _ => unreachable!(),
                }
            }

            // Otherwise, generate any valid Unicode scalar except " and \
            loop {
                match g.gen::<char>() {
                    '"' | '\\' => {},
                    ch => break Unescaped(ch),
                }
            }
        });

        ValidString { chars: it.collect() }
    }

    fn shrink(&self) -> Box<Iterator<Item=Self>> {
        let it = (0..self.chars.len()).map(|i| {
            let chars = self.chars
                .iter()
                .enumerate()
                .filter(|&(j, _)| j != i)
                .map(|(_, c)| *c)
                .collect();

            ValidString { chars }
        });

        Box::new(it.collect::<Vec<_>>().into_iter())
    }
}

impl Lexeme for ValidString {
    fn render(&self, buf: &mut String, end: &mut lexer::Location) -> lexer::TokenKind {
        use CharacterLiteral::*;

        let old_len = buf.len();

        buf.reserve(self.chars.len() * 2);
        buf.push('"');

        for ch in &self.chars {
            match *ch {
                Unescaped(uch) => buf.push(uch),
                Backslash      => buf.push_str(r#"\\"#),
                Quote          => buf.push_str(r#"\""#),
                Cr             => buf.push_str(r#"\r"#),
                Lf             => buf.push_str(r#"\n"#),
                Tab            => buf.push_str(r#"\t"#),
                Hex(byte)      => write!(buf, "\\x{:02x}", byte).unwrap(),
                Unicode(uch)   => write!(buf, "\\u{{{:x}}}", uch as u32).unwrap(),
            }
        }

        buf.push('"');

        for g in buf[old_len..].graphemes(true) {
            // if one char is vertical whitespace, then all of them must be so
            if g.contains(VER_WS) {
                assert!(g.chars().all(|ch| VER_WS.contains(&ch)));
                end.line += 1;
                end.column = 1;
            } else {
                end.column += 1;
            }
        }

        lexer::TokenKind::StringLiteral
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

// Valid operators
static PUNCTUATION: &[&str] = &[
     "<->",  "<->!",  "<->?",  "<->*",  "<->+",
    "!<->", "!<->!", "!<->?", "!<->*", "!<->+",
    "?<->", "?<->!", "?<->?", "?<->*", "?<->+",
    "*<->", "*<->!", "*<->?", "*<->*", "*<->+",
    "+<->", "+<->!", "+<->?", "+<->*", "+<->+",

    "...", "..", ".", "<=", ">=", "<", ">", "==", "!=",
    "->", "=>", "=", "::", ":", "(", ")", "[", "]", "{", "}",
    "+", "-", "*", "/", "%", "&", "|", "~", "?", ",", ";",
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
        static ref re: Regex = Regex::new(r"^[_\p{XID_Start}]$").unwrap();
    }

    re.is_match(ch.encode_utf8(&mut [0; CHAR_MAX_BYTES]))
}

#[allow(non_upper_case_globals)]
fn is_ident_cont(ch: char) -> bool {
    // UnicodeXID::is_xid_continue(ch)

    lazy_static! {
        static ref re: Regex = Regex::new(r"^\p{XID_Continue}$").unwrap();
    }

    re.is_match(ch.encode_utf8(&mut [0; CHAR_MAX_BYTES]))
}

fn shrunk_transitive_closure<T, I>(it: I) -> vec::IntoIter<T>
    where T: Arbitrary,
          I: Iterator<Item=T> {

    let result = it.flat_map(|item| {
        let shrunk_items = item.shrink();
        iter::once(item).chain(shrunk_transitive_closure(shrunk_items))
    });

    result.collect::<Vec<_>>().into_iter()
}

//
// Actual Unit Tests
//

#[test]
fn shrink_valid_whitespace_items() {
    let ws = ValidWhitespace {
        buf: " \r\n".to_owned()
    };

    let actual: Vec<_> = shrunk_transitive_closure(iter::once(ws))
        .map(|lexeme| lexeme.buf)
        .collect();

    let expected = [
        " \r\n",
        "\r\n", "\n", "\r",
        " \n",  "\n", " ",
        " \r",  "\r", " ",
    ];

    assert_eq!(actual, expected);
}

#[test]
fn shrink_valid_whitespace_size() {
    fn num_shrunk(len: usize) -> usize {
        match len {
            0 => panic!("Valid whitespace must be at least one char long"),
            1 => 1,
            _ => 1 + len * num_shrunk(len - 1),
        }
    }

    let mut g = quickcheck::StdGen::new(rand::OsRng::new().unwrap(), 100);

    for _ in 0..100 {
        let ws = ValidWhitespace::arbitrary(&mut g);
        let num_chars = ws.buf.chars().count();

        // this blows up exponentially, so only check for length < 10
        if num_chars >= 10 { continue }

        let num_actual = shrunk_transitive_closure(iter::once(ws.clone())).count();
        let num_expected = num_shrunk(num_chars);

        assert_eq!(num_actual, num_expected, "{:?}", ws);
    }
}

#[test]
fn shrink_valid_line_comment_items() {
    let comment = ValidLineComment {
        buf: "#XYZ\n".to_owned(),
    };

    let actual: Vec<_> = shrunk_transitive_closure(iter::once(comment))
        .map(|lexeme| lexeme.buf)
        .collect();

    let expected = [
        "#XYZ\n",
        "#YZ\n",  "#Z\n", "#\n", "#Y\n", "#\n",
        "#XZ\n",  "#Z\n", "#\n", "#X\n", "#\n",
        "#XY\n",  "#Y\n", "#\n", "#X\n", "#\n",
    ];

    assert_eq!(actual, expected);
}

#[test]
fn shrink_valid_line_comment_size() {
    fn num_shrunk(len: usize) -> usize {
        match len {
            0 | 1 => panic!("Valid line comment must be at least 2 chars long (#\n)"),
            2     => 1,
            _     => 1 + (len - 2) * num_shrunk(len - 1),
        }
    }

    let mut g = quickcheck::StdGen::new(rand::OsRng::new().unwrap(), 100);

    for _ in 0..100 {
        let comment = ValidLineComment::arbitrary(&mut g);
        let num_chars = comment.buf.chars().count();

        // this blows up exponentially, so only check for length < 12
        if num_chars >= 12 { continue }

        let num_actual = shrunk_transitive_closure(iter::once(comment.clone())).count();
        let num_expected = num_shrunk(num_chars);

        assert_eq!(num_actual, num_expected, "{:?}", comment);
    }
}

#[test]
fn shrink_valid_word_items() {
    let word = ValidWord {
        buf: "a1b2".to_owned(),
    };

    let actual: Vec<_> = shrunk_transitive_closure(iter::once(word))
        .map(|lexeme| lexeme.buf)
        .collect();

    let expected = [
        "a1b2",
        "ab2", "a2", "a", "ab", "a",
        "a12", "a2", "a", "a1", "a",
        "a1b", "ab", "a", "a1", "a",
    ];

    assert_eq!(actual, expected);
}

#[test]
fn shrink_valid_word_size() {
    fn num_shrunk(len: usize) -> usize {
        match len {
            0 => panic!("Valid word must be at least one char long"),
            1 => 1,
            _ => 1 + (len - 1) * num_shrunk(len - 1),
        }
    }

    let mut g = quickcheck::StdGen::new(rand::OsRng::new().unwrap(), 100);

    for _ in 0..100 {
        let word = ValidWord::arbitrary(&mut g);
        let num_chars = word.buf.chars().count();

        // this blows up exponentially, so only check for length < 10
        if num_chars >= 10 { continue }

        let num_actual = shrunk_transitive_closure(iter::once(word.clone())).count();
        let num_expected = num_shrunk(num_chars);

        assert_eq!(num_actual, num_expected, "{:?}", word);
    }
}

#[test]
fn shrink_valid_number_items() {
    let numbers = vec![
        (
            ValidNumber {
                buf: "908".to_owned(),
                kind: ValidNumberKind::DecimalInt,
                prefix_len: 0,
            },
            vec![
                "908",
                "08", "8", "0",
                "98", "8", "9",
                "90", "0", "9",
            ],
        ),
        (
            ValidNumber {
                buf: "0b110".to_owned(),
                kind: ValidNumberKind::BinaryInt,
                prefix_len: 2,
            },
            vec![
                "0b110",
                "0b10", "0b0", "0b1",
                "0b10", "0b0", "0b1",
                "0b11", "0b1", "0b1",
            ],
        ),
        (
            ValidNumber {
                buf: "0o704".to_owned(),
                kind: ValidNumberKind::OctalInt,
                prefix_len: 2,
            },
            vec![
                "0o704",
                "0o04", "0o4", "0o0",
                "0o74", "0o4", "0o7",
                "0o70", "0o0", "0o7",
            ],
        ),
        (
            ValidNumber {
                buf: "0x30f".to_owned(),
                kind: ValidNumberKind::HexInt,
                prefix_len: 2,
            },
            vec![
                "0x30f",
                "0x0f", "0xf", "0x0",
                "0x3f", "0xf", "0x3",
                "0x30", "0x0", "0x3",
            ],
        ),
        (
            ValidNumber {
                buf: "123.456e+78".to_owned(),
                kind: ValidNumberKind::FloatingPoint,
                prefix_len: 0,
            },
            vec!["123.456e+78"],
        ),
    ];

    for (number, expected) in numbers {
        let actual: Vec<_> = shrunk_transitive_closure(iter::once(number))
            .map(|lexeme| lexeme.buf)
            .collect();

        assert_eq!(actual, expected);
    }
}

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

#[test]
fn all_valid_punctuation() {
    for punct in PUNCTUATION {
        let range = lexer::Range {
            begin: lexer::Location { line: 1, column: 1, src_idx: 0 },
            end:   lexer::Location { line: 1, column: punct.len() + 1, src_idx: 0 },
        };
        let sources = &[punct];
        let tokens = lexer::lex(sources).unwrap();
        let token = tokens[0];

        assert_eq!(tokens.len(), 1);
        assert_eq!(token.kind, lexer::TokenKind::Punctuation);
        assert_eq!(token.value, *punct);
        assert_eq!(token.range, range);
    }
}

#[test]
fn windows_newline_in_comment() {
    let src = ValidSource {
        files: vec![
            vec![
                ValidToken::LineComment(
                    ValidLineComment {
                        buf: "#abc\r\n".to_owned()
                    }
                ),
            ]
        ]
    };
    let (srcs, _) = src.render();
    let actual = lexer::lex(&srcs).unwrap();

    assert_eq!(actual.len(), 1);
    assert_eq!(actual[0].kind, lexer::TokenKind::Comment);
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
        let (sources, expected) = source.render();
        let actual = lexer::lex(&sources).unwrap();

        assert_eq!(actual.len(), expected.len());

        for (act_tok, exp_tok) in actual.iter().zip(expected) {
            let src_idx = exp_tok.range.begin.src_idx;
            let slice = exp_tok.slice.clone();

            assert_eq!(act_tok.kind, exp_tok.kind);
            assert_eq!(act_tok.value, &sources[src_idx][slice]);
            assert_eq!(act_tok.range, exp_tok.range);
        }

        true
    }
}
