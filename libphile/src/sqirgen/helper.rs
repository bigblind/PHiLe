//
// sqirgen/helper.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 08/11/2017
//

//! Macros, functions, and types used internally by `SqirGen`.

use std::char;
use std::str::Chars;
use std::collections::HashMap;
use sqir::{ Type, RcType, RcExpr, Cardinality };
use error::{ Result, Error };
use util::{ Range, Ranged, RcCell, SkipN };

/// This is to be used ONLY when you _know_
/// you have an entity (currently, a Class) type
pub fn unwrap_entity_name(entity: &RcType) -> Result<String> {
    match *entity.borrow()? {
        Type::Class(ref c) => Ok(c.name.clone()),
        ref ty => bug!("Non-class entity type?! {}", ty),
    }
}

/// Gets the range of the `enum`, `struct`, or `class` type.
/// Returns an error if `ty` is not such a user-defined type.
pub fn unwrap_ud_type_range(ty: &RcType) -> Result<Range> {
    let range = match *ty.borrow()? {
        Type::Enum(ref e)   => e.range,
        Type::Struct(ref s) => s.range,
        Type::Class(ref c)  => c.range,
        _ => bug!("Type {} doesn't have a range", ty)?,
    };
    Ok(range)
}

/// Unescapes a string literal and strips delimiting quotes.
pub fn parse_string_literal(lexeme: &str, range: Range) -> Result<String> {
    if !lexeme.starts_with('"') || !lexeme.ends_with('"') || lexeme.len() < 2 {
        bug!("Missing leading or trailing \" in string at {}", range)?
    }

    let mut chars = lexeme[1..lexeme.len() - 1].chars();
    let mut buf = String::with_capacity(lexeme.len());
    let unterm_err = lazy_bug!("Unterminated escape sequence in string at {}", range);

    // TODO(H2CO3): keep this in sync with the lexer
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            match chars.next().ok_or_else(&unterm_err)? {
                '\\' => buf.push('\\'),
                '"' => buf.push('\"'),
                'n' => buf.push('\n'),
                'r' => buf.push('\r'),
                't' => buf.push('\t'),
                'x' => buf.push(unescape_hex(&mut chars, range)?),
                'u' => buf.push(unescape_unicode(&mut chars, range)?),
                esc => bug!("Invalid escape \\{} in string at {}", esc, range)?,
            }
        } else {
            buf.push(ch); // unescaped
        }
    }

    Ok(buf)
}

// Helper for parse_string_literal().
// TODO(H2CO3): keep this in sync with the lexer.
fn unescape_hex(chars: &mut Chars, range: Range) -> Result<char> {
    // exactly 2 hex digits following the \x must have been picked up by the lexer
    const NUM_DIGITS: usize = 2;
    // \x escapes must denote Unicode scalars strictly less than 128
    const MAX_ALLOWED: u8 = 0b1000_0000;

    // hex characters are ASCII, so one byte == one `char`
    let payload = chars.as_str().get(..NUM_DIGITS).ok_or_else(
        lazy_bug!("Expected 2 hex digits after \\x in string at {}", range)
    )?;
    let code_point = u8::from_str_radix(payload, 16).or_else(
        |err| bug!("Non-hex digits {} in string at {} ({})", payload, range, err)
    )?;

    // skip the hex characters
    chars.skip_n(NUM_DIGITS);

    // the range, however, cannot be/is not checked in the lexer
    if code_point < MAX_ALLOWED {
        Ok(code_point as char)
    } else {
        sema_error!(
            range,
            "Invalid escape \\x{:02x}: must be in range [0x00..0x{:02x})",
            code_point,
            MAX_ALLOWED,
        )
    }
}

// Helper for parse_string_literal().
// TODO(H2CO3): keep this in sync with the lexer.
fn unescape_unicode(chars: &mut Chars, range: Range) -> Result<char> {
    if chars.next() != Some('{') {
        bug!("digits in \\u escape must be within {{}}s (string at {})", range)?
    }

    let payload = chars.as_str();
    let pos = payload.find('}').ok_or_else(
        lazy_bug!("digits in \\u escape must be within {{}}s (string at {})", range)
    )?;

    // The hex number fitting into an u32 can't be/isn't checked by the lexer
    let code_point = u32::from_str_radix(&payload[..pos], 16).or_else(
        |err| sema_error!(range, "Invalid \\u escape: {}", err)
    )?;

    // Skip the `payload` _and_ the subsequent closing brace.
    // This works with the byte `pos`ition, as they are all ASCII.
    chars.skip_n(pos + 1);

    // It is not checked by the lexer whether the number is a valid Unicode scalar
    match char::from_u32(code_point) {
        Some(ch) => Ok(ch),
        None => sema_error!(range, "U+{:04X} isn't a valid Unicode scalar", code_point),
    }
}

/// Invalid bool literals should have been caught by the lexer/parser.
/// Therefore if this encounters something other
/// than `"true"` or `"false"`, it returns an ICE.
pub fn parse_bool_literal(lexeme: &str, range: Range) -> Result<bool> {
    lexeme.parse().or_else(
        |_| bug!("Invalid boolean literal {} at {}", lexeme, range)
    )
}

/// This returns a `Semantic` error and not a `[lazy_]bug!()`, because
/// overflowing floats can't be/aren't validated in the lexer/parser.
pub fn parse_float_literal(lexeme: &str, range: Range) -> Result<f64> {
    lexeme.parse().or_else(
        |err| sema_error!(range, "Invalid float literal {}: {}", lexeme, err)
    )
}

/// Attempts to parse the string representation of an integer
/// literal into an actual unsigned integer.
pub fn parse_int_literal(lexeme: &str, range: Range) -> Result<u64> {
    let radix_map = [
        ("0b",  2),
        ("0B",  2),
        ("0o",  8),
        ("0O",  8),
        ("0x", 16),
        ("0X", 16),
    ];

    for &(prefix, radix) in &radix_map {
        if lexeme.starts_with(prefix) {
            return parse_int_with_radix(&lexeme[prefix.len()..], radix, range);
        }
    }

    parse_int_with_radix(lexeme, 10, range)
}

// Helper for parse_int_literal().
// This returns a `Semantic` error and not a `[lazy_]bug!()`, because
// overflowing integers can't be/aren't validated in the lexer/parser.
fn parse_int_with_radix(lexeme: &str, radix: u32, range: Range) -> Result<u64> {
    u64::from_str_radix(lexeme, radix).or_else(
        |err| sema_error!(range, "Invalid base-{} integer {}: {}", radix, lexeme, err)
    )
}

/// Invalid operators should have been caught by the lexer/parser.
/// Consequently, this returns an ICE upon encountering one.
pub fn parse_cardinality_op(op: &str, range: Range) -> Result<(Cardinality, Cardinality)> {
    let op_error = lazy_bug!("Invalid cardinality operator {} at {}", op, range);

    let cardinality_from_char = |ch| Ok(match ch {
        '<' => Cardinality::One,
        '>' => Cardinality::One,
        '?' => Cardinality::ZeroOrOne,
        '!' => Cardinality::One,
        '*' => Cardinality::ZeroOrMore,
        '+' => Cardinality::OneOrMore,
        _   => return Err(op_error()),
    });

    // TODO(H2CO3): &op_error should be op_error once #1369 is
    // resolved (https://github.com/rust-lang/rfcs/issues/1369)
    let mut chars = op.chars();
    let first = chars.next().ok_or_else(&op_error)?;
    let last = chars.next_back().ok_or_else(&op_error)?;
    let lhs = cardinality_from_char(first)?;
    let rhs = cardinality_from_char(last)?;

    Ok((lhs, rhs))
}

/// A "typing context": an optional type hint, along with location
///  information. Used for the top-down part of type inference.
#[derive(Debug, Clone)]
pub struct TyCtx {
    /// The type hint, if available.
    pub ty: Option<RcType>,
    /// The source range of the expression to be inferred.
    pub range: Range,
}

impl Ranged for TyCtx {
    fn range(&self) -> Range {
        self.range
    }
}

/// RAII guard that ensures that locals go out of scope correctly.
#[derive(Debug)]
pub struct ScopeGuard {
    /// A pointer to the local variables stored inside SqirGen.
    pub locals: RcCell<Locals>,
}

impl Drop for ScopeGuard {
    fn drop(&mut self) {
        let mut locals = self.locals.borrow_mut().expect("can't borrow locals");
        let scope = locals.scope_stack.pop().expect("no innermost scope found");

        // TODO(H2CO3): if we ever implement destructors, then
        // destroy local variables in _reverse_ order of init.
        for var_name in &scope {
            locals.var_map.remove(var_name).expect("variable not in declaration map");
        }
    }
}

/// Stores the local variables declared at a particular
/// point in the process of generating SQIR.
#[derive(Debug, Default)]
pub struct Locals {
    /// The map of all currently-visible local variables, transitively.
    /// That is, `var_map.keys()` ~ `scope_stack.flat_map(id)`.
    pub var_map: HashMap<String, RcExpr>,
    /// The vector of currently-active scopes: the higher the index,
    /// the smaller/inner the corresponding scope is. Within scope
    /// vectors, variable names are stored in order of declaration.
    pub scope_stack: Vec<Vec<String>>,
}
