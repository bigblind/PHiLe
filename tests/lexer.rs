//
// tests/lexer.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 31/07/2017
//

use std::str;
use std::ops;
use quickcheck::{ Arbitrary, Gen };
use phile::error::Error;
use phile::lexer;


// Types for generating correct and incorrect source code
#[derive(Debug, Clone)]
struct GoodSource {
    buf: String,
    slices: Vec<ops::Range<usize>>,
    ranges: Vec<lexer::Range>,
    src_idx: usize,
}

impl GoodSource {
    fn new() -> GoodSource {
        GoodSource {
            buf: String::new(),
            slices: Vec::new(),
            ranges: Vec::new(),
            src_idx: 0,
        }
    }

    fn end_location(&self) -> lexer::Location {
        self.ranges.last().map_or(
            lexer::Location { line: 1, column: 1, src_idx: self.src_idx },
            |range| range.end,
        )
    }

    fn push(&mut self, s: &str, end: lexer::Location) {
        let begin = self.end_location();
        self.slices.push(self.buf.len()..self.buf.len() + s.len());
        self.buf.push_str(s);
        self.ranges.push(lexer::Range { begin, end });
    }

    fn push_white_space<G: Gen>(&mut self, g: &mut G) {
        #[derive(Debug, Clone, Copy)]
        struct Ws {
            ch: char,
            line_break: bool,
        }

        #[allow(non_upper_case_globals)]
        static ws: &[Ws] = &[
            Ws { ch: '\u{0009}', line_break: false },
            Ws { ch: '\u{000A}', line_break: true  },
            Ws { ch: '\u{000B}', line_break: true  },
            Ws { ch: '\u{000C}', line_break: true  },
            Ws { ch: '\u{000D}', line_break: true  },
            Ws { ch: '\u{0020}', line_break: false },
            Ws { ch: '\u{0085}', line_break: true  },
            Ws { ch: '\u{00A0}', line_break: false },
            Ws { ch: '\u{1680}', line_break: false },
            Ws { ch: '\u{2000}', line_break: false },
            Ws { ch: '\u{2001}', line_break: false },
            Ws { ch: '\u{2002}', line_break: false },
            Ws { ch: '\u{2003}', line_break: false },
            Ws { ch: '\u{2004}', line_break: false },
            Ws { ch: '\u{2005}', line_break: false },
            Ws { ch: '\u{2006}', line_break: false },
            Ws { ch: '\u{2007}', line_break: false },
            Ws { ch: '\u{2008}', line_break: false },
            Ws { ch: '\u{2009}', line_break: false },
            Ws { ch: '\u{200A}', line_break: false },
            Ws { ch: '\u{2028}', line_break: true  },
            Ws { ch: '\u{2029}', line_break: true  },
            Ws { ch: '\u{202F}', line_break: false },
            Ws { ch: '\u{205F}', line_break: false },
            Ws { ch: '\u{3000}', line_break: false },
        ];

        #[allow(non_upper_case_globals)]
        const max_num_chars: usize = 8;

        #[allow(non_upper_case_globals)]
        const max_char_bytes: usize = 4;

        // generate at least 1, at most max_num_chars whitespace characters
        let mut end = self.end_location();
        let mut buf = [0u8; max_num_chars * max_char_bytes];
        let mut n_bytes = 0;
        let num_chars = g.gen_range::<usize>(0 + 1, max_num_chars + 1);

        for _ in 0..num_chars {
            let w = ws[g.gen_range(0, ws.len())];

            n_bytes += w.ch.encode_utf8(&mut buf[n_bytes..]).len();

            if w.line_break {
                end.column = 1;
                end.line += 1;
            } else {
                end.column += 1;
            }
        }

        self.push(str::from_utf8(&mut buf[..n_bytes]).unwrap(), end);
    }
}

impl Arbitrary for GoodSource {
    fn arbitrary<G: Gen>(g: &mut G) -> GoodSource {
        let mut src = GoodSource::new();
        src.push_white_space(g);
        src
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
    assert_eq!(lexer::lex(&["012"]).unwrap()[0].kind, lexer::TokenKind::NumericLiteral);
    assert_eq!(lexer::lex(&["102"]).unwrap()[0].kind, lexer::TokenKind::NumericLiteral);
    assert_eq!(lexer::lex(&["0.34"]).unwrap()[0].kind, lexer::TokenKind::NumericLiteral);
    assert_eq!(lexer::lex(&["4.56"]).unwrap()[0].kind, lexer::TokenKind::NumericLiteral);
    assert_eq!(lexer::lex(&["1.57e9"]).unwrap()[0].kind, lexer::TokenKind::NumericLiteral);
    assert_eq!(lexer::lex(&["24.6E-08"]).unwrap()[0].kind, lexer::TokenKind::NumericLiteral);
}

#[test]
fn unicode_digit_is_not_numeric() {
    assert_eq!(lexer::lex(&["\u{0660}"]).unwrap()[0].kind, lexer::TokenKind::Word);
    assert_eq!(lexer::lex(&["\u{0967}"]).unwrap()[0].kind, lexer::TokenKind::Word);
    assert_eq!(lexer::lex(&["\u{0e52}"]).unwrap()[0].kind, lexer::TokenKind::Word);
    assert_eq!(lexer::lex(&["\u{0967}"]).unwrap()[0].kind, lexer::TokenKind::Word);
    assert_eq!(lexer::lex(&["\u{1814}"]).unwrap()[0].kind, lexer::TokenKind::Word);
    assert_eq!(lexer::lex(&["\u{ff15}"]).unwrap()[0].kind, lexer::TokenKind::Word);
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
            Err(_) => false,
            Ok(tokens) => sources.iter().all(String::is_empty) || !tokens.is_empty(),
        }
    }

    #[allow(trivial_casts)]
    fn good_whitespace(source: GoodSource) -> bool {
        let sources = &[source.buf];
        let tokens = lexer::lex(sources).unwrap();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, lexer::TokenKind::Whitespace);
        assert_eq!(&tokens[0].range, source.ranges.last().unwrap());

        true
    }
}
