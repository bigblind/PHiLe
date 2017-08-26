//
// util.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/05/2017
//

use std::rc::{ Rc, Weak };
use std::cell::{ RefCell, Ref, RefMut };
use std::hash::{ Hash, Hasher };
use std::fmt::{ self, Display, Formatter };
use error::{ Error, Result };
use unicode_segmentation::UnicodeSegmentation;


/// Generic macro for building an associated container literal.
macro_rules! assoc_map {
    ($t: ident, $($k: expr => $v: expr),*) => ({
        let mut _tmp = ::std::collections::$t::new();
        $({
            let key = $k;
            let val = $v;
            _tmp.insert(key.into(), val.into()).map(
                |_| panic!("duplicate value for key {:#?}", key)
            );
        })*
        _tmp
    })
}

/// Builds an `std::collections::BTreeMap<K, V>` from keys and values
/// so that `K: From<KeyType>` and `V: From<ValueType>` for the types
/// of each key and value, respectively. A key-value pair must have
/// the format `key => value`, and key-value pairs are separated by
/// commas. A trailing comma is permitted and will be ignored.
macro_rules! btree_map {
    ($($k: expr => $v: expr),*) => {
        assoc_map!(BTreeMap, $($k => $v),*)
    };
    ($($k: expr => $v: expr),+,) => { btree_map!($($k => $v),+) };
}

/// Type of a global descriptor that holds information about
/// the current version of the PHiLe package (library and compiler).
/// The purpose of a global instance of this struct is to provide
/// user-readable version information in a uniform manner throughout
/// the code base, and for potential 3rd-party tooling.
#[derive(Debug, Clone, Copy)]
pub struct PackageInfo {
    /// The name of the PHiLe package.
    pub name:        &'static str,
    /// The version of the PHiLe package.
    pub version:     &'static str,
    /// The list of authors of PHiLe.
    pub authors:     &'static str,
    /// A short summary of this package.
    pub description: &'static str,
    /// URL of the PHiLe home page.
    pub home_page:   &'static str,
}

/// A struct that contains escape sequences for ANSI terminal color
/// codes. This is used for maintaining a uniform and consistent
/// color scheme across diagnostic messages printed by the PHiLe
/// compiler and 3rd-party tools.
#[derive(Debug, Clone, Copy)]
pub struct Color {
    /// ANSI escape sequence for resetting all color settings.
    pub reset:     &'static str,
    /// ANSI escape sequence for displaying informative messages.
    pub info:      &'static str,
    /// ANSI escape sequence for displaying highlighted messages.
    pub highlight: &'static str,
    /// ANSI escape sequence for indicating successful compilation.
    pub success:   &'static str,
    /// ANSI escape sequence for reporting errors.
    pub error:     &'static str,
}

/// A reference counted, dynamically borrow checked smart pointer.
/// Like `Rc<RefCell<T>>`, but with a more convenient interface.
#[derive(Debug, Default)]
pub struct RcCell<T: ?Sized> {
    ptr: Rc<RefCell<T>>,
}

/// Weak counterpart of `RcCell<T>`.
#[derive(Debug, Default)]
pub struct WkCell<T: ?Sized> {
    ptr: Weak<RefCell<T>>,
}

/// Holds metadata about the PHiLe package as defined in the Cargo manifest.
pub static PACKAGE_INFO: PackageInfo = PackageInfo {
    name:        env!["CARGO_PKG_NAME"],
    version:     env!["CARGO_PKG_VERSION"],
    authors:     env!["CARGO_PKG_AUTHORS"],
    description: env!["CARGO_PKG_DESCRIPTION"],
    home_page:   env!["CARGO_PKG_HOMEPAGE"],
};

/// The definition of the PHiLe diagnostic color scheme.
pub static COLOR: Color = Color {
    reset:     "\x1b[0m",
    info:      "\x1b[1;33m",
    highlight: "\x1b[1;36m",
    success:   "\x1b[1;32m",
    error:     "\x1b[1;31m",
};


/// Returns the number of extended grapheme clusters in `string`.
/// Useful for counting 'characters' in accordance with a user's
/// notion of a 'character' or grapheme. Mainly used by the lexer
/// for generating visually accurate source location data.
///
/// # Arguments:
///
/// * `string`: a string slice.
///
/// # Return value:
///
/// The number of extended grapheme clusters in `string`.
pub fn grapheme_count(string: &str) -> usize {
    string.graphemes(true).count()
}

/// Counts the grapheme clusters in a string that satisfy a condition.
///
/// # Arguments:
///
/// * `string`: a string slice.
/// * `pred`: a predicate function invoked for each extended grapheme
///   cluster in `string`.
///
/// # Return value:
///
/// The number of extended grapheme clusters in `string`
/// for which `pred` returned `true`.
pub fn grapheme_count_by<P: Fn(&str) -> bool>(string: &str, pred: P) -> usize {
    string.graphemes(true).filter(|g| pred(*g)).count()
}

/// Parses escape sequences in a string literal and returns the
/// 'raw' string that is represented by the (escaped) input.
/// Also removes the leading and trailing quotation marks.
///
/// # Arguments:
///
/// * `string`: a potentially-escaped string literal in PHiLe syntax.
///
/// # Return value:
///
/// * `Ok(String)`: The unescaped, quote-trimmed raw string if
///   the string literal is in the correct format.
/// * `Err(Error::Syntax)`, if the string literal contained an
///   invalid escape sequence (or any other syntactic error).
pub fn unescape_string_literal(string: &str) -> Result<String> {
    if string.contains('\\') {
        unimplemented!() // TODO(H2CO3): unescape string literals
    } else {
        Ok(string.to_owned())
    }
}


impl<T> RcCell<T> {
    /// Creates an `RcCell` owning the `value`.
    pub fn new(value: T) -> RcCell<T> {
        RcCell {
            ptr: Rc::new(RefCell::new(value))
        }
    }

    /// Attempts to immutably borrow the pointed value.
    ///
    /// # Return value:
    ///
    /// `Ok(Ref<T>)`, if an immutable borrow is dynamically possible.
    /// `Err(Error::Borrow)`, if an immutable borrow is dynamically
    /// impossible, e.g. because there is outstanding mutable loan.
    pub fn borrow(&self) -> Result<Ref<T>> {
        self.ptr.try_borrow().map_err(From::from)
    }

    /// Attempts to mutably borrow the pointed value.
    ///
    /// # Return value:
    ///
    /// `Ok(Ref<T>)`, if a mutable borrow is dynamically possible.
    /// `Err(Error::BorrowMyut)`, if a mutable borrow is dynamically
    /// impossible, e.g. because there is outstanding immutable loan.
    pub fn borrow_mut(&self) -> Result<RefMut<T>> {
        self.ptr.try_borrow_mut().map_err(From::from)
    }

    /// Converts the strong pointer to a weak pointer.
    pub fn as_weak(&self) -> WkCell<T> {
        WkCell {
            ptr: Rc::downgrade(&self.ptr)
        }
    }

    /// Returns the number of strong pointers pointing to the inner value.
    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self.ptr)
    }

    /// Returns the number of weak pointers pointing to the inner value.
    pub fn weak_count(&self) -> usize {
        Rc::weak_count(&self.ptr)
    }
}

/// Clones the pointer only, such that the returned strong
/// pointer points to the same value as `self`.
impl<T> Clone for RcCell<T> {
    fn clone(&self) -> RcCell<T> {
        RcCell {
            ptr: self.ptr.clone()
        }
    }
}

// TODO(H2CO3): implement this once { Unsize, CoerceUnsized } are stable
// impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<RcCell<U>> for RcCell<T> {}

/// Tests equality based on pointer identity.
impl<T> PartialEq for RcCell<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.ptr, &other.ptr)
    }
}

/// Tests equality based on pointer identity.
impl<T> Eq for RcCell<T> {}

/// Hashes the pointer address itself.
impl<T> Hash for RcCell<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.ptr.as_ptr().hash(hasher)
    }
}

impl<T> Display for RcCell<T> where T: Display {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.borrow() {
            Ok(ptr) => ptr.fmt(f),
            Err(_)  => f.write_str("<cannot borrow RcCell>"),
        }
    }
}

impl<T> From<T> for RcCell<T> {
    fn from(value: T) -> RcCell<T> {
        RcCell::new(value)
    }
}

impl<T> WkCell<T> {
    /// Creates a `WkCell` that doesn't refer to any value.
    /// `as_rc()` will always return an `Err` for such `WkCell`s.
    pub fn new() -> WkCell<T> {
        WkCell {
            ptr: Weak::new()
        }
    }

    /// Converts a weak pointer to a strong pointer if possible.
    ///
    /// # Return value:
    ///
    /// * `Ok(RcCell<T>)` if `self` points to a still-existing value.
    /// * `Err(Error::Strongify)` if `self` pointed to a now-deallocated value.
    pub fn as_rc(&self) -> Result<RcCell<T>> {
        self.ptr.upgrade().map(|rc| RcCell { ptr: rc }).ok_or(Error::Strongify)
    }
}

/// Clones the weak pointer so that the returned result points to
/// the same value as `self` does, if any. If `self` doesn't point
/// anywhere, the clone will not be able to be strongified either.
impl<T> Clone for WkCell<T> {
    fn clone(&self) -> WkCell<T> {
        WkCell {
            ptr: self.ptr.clone()
        }
    }
}

impl<T> Display for WkCell<T> where RcCell<T>: Display {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.as_rc() {
            Ok(rc) => rc.fmt(f),
            Err(_) => f.write_str("<cannot strongify WkCell>"),
        }
    }
}
