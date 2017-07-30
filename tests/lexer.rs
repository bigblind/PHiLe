//
// tests/lexer.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 30/07/2017
//

#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications)]

#[macro_use]
extern crate quickcheck;
extern crate phile;


#[cfg(test)]
mod tests {
    use phile::error::Error;
    use phile::lexer::lex;

    #[test]
    fn no_sources() {
        let sources: &[&str] = &[];
        assert!(lex(sources).unwrap().is_empty());
    }

    #[test]
    fn single_empty_source() {
        assert!(lex(&[""]).unwrap().is_empty());
    }

    #[test]
    fn multiple_empty_sources() {
        assert!(lex(&["", ""]).unwrap().is_empty());
    }

    quickcheck! {
        #[allow(trivial_casts)]
        fn random_sources(sources: Vec<String>) -> bool {
            match lex(&sources) {
                Err(Error::Syntax { ref message, range, .. }) => {
                    assert_eq!(message, "Invalid token");
                    assert!(!sources[range.unwrap().begin.src_idx].is_empty());
                    true
                },
                Err(_) => false,
                Ok(tokens) => sources.iter().all(String::is_empty) || !tokens.is_empty(),
            }
        }
    }
}
