//
// tests/common/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 10/11/2017
//

#![deny(missing_debug_implementations, missing_copy_implementations,
        trivial_casts, trivial_numeric_casts,
        unsafe_code,
        unstable_features,
        unused_import_braces, unused_qualifications)]
#![cfg_attr(feature = "cargo-clippy",
            allow(match_same_arms, clone_on_ref_ptr))]
#![cfg_attr(feature = "cargo-clippy",
            deny(wrong_pub_self_convention, used_underscore_binding,
                 stutter, similar_names, pub_enum_variant_names,
                 non_ascii_literal, unicode_not_nfc,
                 /* result_unwrap_used, option_unwrap_used, */ // TODO(H2CO3): fix these
                 option_map_unwrap_or_else, option_map_unwrap_or, filter_map,
                 shadow_unrelated, shadow_reuse, shadow_same,
                 int_plus_one, string_add_assign, if_not_else,
                 invalid_upcast_comparisons,
                 cast_sign_loss, cast_precision_loss,
                 cast_possible_wrap, cast_possible_truncation,
                 mutex_integer, mut_mut, items_after_statements,
                 print_stdout, mem_forget, maybe_infinite_iter))]

use regex::Regex;
use phile::util::{ Location, Range };


#[derive(Debug)]
pub struct InvalidTestCase {
    pub source:  &'static str,
    pub marker:  &'static str,
    pub message: &'static str,
}

impl InvalidTestCase {
    #[allow(non_upper_case_globals)]
    pub fn error_range(&self) -> Range {
        lazy_static! {
            static ref regex: Regex = Regex::new(r"^ *(\^_*\^) *$").unwrap();
        }

        let m = regex.captures(self.marker).unwrap().get(1).unwrap();
        let start_index = 1 + m.start();
        let end_index = 1 + m.end() - 1;

        oneline_range(0, start_index..end_index)
    }
}

#[cfg_attr(feature = "cargo-clippy", allow(needless_pass_by_value))]
pub fn oneline_range(src_idx: usize, char_range: ::std::ops::Range<usize>) -> Range {
    Range {
        start: Location { src_idx, line: 1, column: char_range.start },
        end:   Location { src_idx, line: 1, column: char_range.end   },
    }
}
