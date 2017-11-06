//
// util.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/05/2017
//

//! The `util` module provides various useful helper functions,
//! types and macros, used extensively throughout the PHiLe
//! source tree. Generic reference-counted smart pointers,
//! string manipulation specific to the syntax of PHiLe, and
//! commonly-used constants are all part of this file.

use std::rc::{ Rc, Weak };
use std::cell::{ RefCell, Ref, RefMut };
use std::hash::{ Hash, Hasher };
use std::fmt::{ self, Display, Formatter };
use error::{ Error, Result };
use unicode_segmentation::UnicodeSegmentation;


/// Generic macro for building an associative container literal.
macro_rules! assoc_map {
    ($ty: ident, $($key: expr => $val: expr),*) => ({
        let mut _tmp = ::std::collections::$ty::new();
        $(
            _tmp.insert($key.into(), $val.into());
        )*
        _tmp
    })
}

/// Builds an `std::collections::BTreeMap<K, V>` from keys and values
/// so that `K: From<KeyType>` and `V: From<ValueType>` for the types
/// of each key and value, respectively. A key-value pair must have
/// the format `key => value`, and key-value pairs are separated by
/// commas. A trailing comma is permitted and will be ignored.
macro_rules! btree_map {
    ($($key: expr => $val: expr),*) => {
        assoc_map!(BTreeMap, $($key => $val),*)
    };
    ($($key: expr => $val: expr),+,) => { btree_map!($($key => $val),+) };
}

/// Type of a global descriptor that holds information about
/// the current version of the PHiLe package (library and compiler).
/// The purpose of a global instance of this struct is to provide
/// user-readable version information in a uniform manner throughout
/// the code base, and for potential 3rd-party tooling.
#[derive(Debug, Clone, Copy)]
pub struct PackageInfo {
    /// The name of the PHiLe package.
    pub name: &'static str,
    /// The version of the PHiLe package.
    pub version: &'static str,
    /// The list of authors of PHiLe.
    pub authors: &'static str,
    /// A short summary of this package.
    pub description: &'static str,
    /// URL of the PHiLe home page.
    pub home_page: &'static str,
}

/// Holds metadata about the PHiLe package as defined in the Cargo manifest.
pub static PACKAGE_INFO: PackageInfo = PackageInfo {
    name:        env!["CARGO_PKG_NAME"],
    version:     env!["CARGO_PKG_VERSION"],
    authors:     env!["CARGO_PKG_AUTHORS"],
    description: env!["CARGO_PKG_DESCRIPTION"],
    home_page:   env!["CARGO_PKG_HOMEPAGE"],
};

/// Used for distinguishing between the types of
/// diagnostic that the compiler can emit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DiagnosticKind {
    /// A message without any special attributes or coloring.
    Default,
    /// An informative message, eg. compilation progress or performance.
    Info,
    /// A highlighted part of a diagnostic.
    Highlight,
    /// Indicates successful compilation.
    Success,
    /// Indicates that an error occurred during compilation.
    Error,
}

/// Returns `DiagnosticKind::Default`.
impl Default for DiagnosticKind {
    fn default() -> Self {
        DiagnosticKind::Default
    }
}

/// A string which, when `Display`ed, looks pretty and colorful.
/// It is used for formatting diagnostic messages.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Diagnostic<T> {
    value: T,
    kind: DiagnosticKind,
}

impl<T> Diagnostic<T> {
    /// Makes a pretty-printable diagnostic that displays
    /// a given value in the specified diagnostic style.
    ///
    /// # Arguments:
    ///
    /// * `value`: the value to be pretty-printed.
    /// * `kind`: the diagnostic style to apply when pretty-printing.
    /// # Return value:
    ///
    /// An initialized `Diagnostic` instance.
    pub fn new(value: T, kind: DiagnosticKind) -> Self {
        Diagnostic { value, kind }
    }

    /// Consumes `self` and returns the inner value, discarding style information.
    pub fn into_inner(self) -> T {
        self.value
    }

    /// Returns the diagnostic kind associated with this instance.
    pub fn kind(&self) -> DiagnosticKind {
        self.kind
    }
}

impl<T> AsRef<T> for Diagnostic<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Diagnostic<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> From<T> for Diagnostic<T> {
    fn from(value: T) -> Self {
        Self::new(value, DiagnosticKind::Default)
    }
}

impl<T> Display for Diagnostic<T> where T: Display {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let reset = "\x1b[0m";
        let color = match self.kind {
            DiagnosticKind::Default   => "",
            DiagnosticKind::Info      => "\x1b[1;33m",
            DiagnosticKind::Highlight => "\x1b[1;36m",
            DiagnosticKind::Success   => "\x1b[1;32m",
            DiagnosticKind::Error     => "\x1b[1;31m",
        };

        write!(f, "{}{}{}{}", reset, color, self.value, reset)
    }
}

/// Represents the location of a single extended grapheme cluster
/// in the sources fed to the lexer.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    /// 0-based index of the source that this location points into.
    pub src_idx: usize,
    /// 1-based line index within the aforementioned source.
    pub line: usize,
    /// 1-based character index within the line.
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "line {}, char {}", self.line, self.column)
    }
}

/// A half-open range representing a source span.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Range {
    /// Location at the beginning of the source range.
    pub start: Location,
    /// Location one past the end of the source range.
    pub end: Location,
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}...{}", self.start, self.end)
    }
}

/// This trait is to be implemented by entities that correspond
/// to some range in the source. This is used for generating
/// location information in user-visible error messages.
pub trait Ranged {
    /// Returns the range `self` was generated from.
    fn range(&self) -> Range;
}

impl Ranged for Range {
    fn range(&self) -> Range {
        *self
    }
}

/// Extends iterators with the `skip_n()` function.
pub trait SkipN: Iterator {
    /// Retrieves and ignores the first `n` items of the iterator. In
    /// other words, this is equivalent with calling `Iterator::nth()`
    /// with `n - 1` if `n > 0`, and is a no-op if `n == 0`.
    /// If there are less than `n` elements remaining in the iterator,
    /// then it is silently exhausted.
    ///
    /// # Arguments:
    ///
    /// * `it`: an iterator of which to skip some elements.
    /// * `n`: the number of items to skip.
    fn skip_n(&mut self, n: usize) {
        for _ in 0..n {
            self.next();
        }
    }
}

impl<T: Iterator> SkipN for T {}

/// A reference counted, dynamically borrow checked smart pointer.
/// Like `Rc<RefCell<T>>`, but with a more convenient interface.
#[derive(Debug, Default)]
pub struct RcCell<T: ?Sized> {
    ptr: Rc<RefCell<T>>,
}

impl<T> RcCell<T> {
    /// Creates an `RcCell` owning the `value`.
    pub fn new(value: T) -> Self {
        RcCell {
            ptr: Rc::new(RefCell::new(value))
        }
    }

    /// Attempts to immutably borrow the pointed value.
    ///
    /// # Return value:
    ///
    /// * `Ok(Ref<T>)`, if an immutable borrow is dynamically possible.
    /// * `Err(Error::Borrow)`, if an immutable borrow is dynamically
    /// impossible, e.g. because there is outstanding mutable loan.
    pub fn borrow(&self) -> Result<Ref<T>> {
        self.ptr.try_borrow().map_err(From::from)
    }

    /// Attempts to mutably borrow the pointed value.
    ///
    /// # Return value:
    ///
    /// * `Ok(Ref<T>)`, if a mutable borrow is dynamically possible.
    /// * `Err(Error::BorrowMut)`, if a mutable borrow is dynamically
    /// impossible, e.g. because there is outstanding immutable loan.
    pub fn borrow_mut(&self) -> Result<RefMut<T>> {
        self.ptr.try_borrow_mut().map_err(From::from)
    }

    /// Converts the strong pointer to a weak pointer.
    pub fn to_weak(&self) -> WkCell<T> {
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
    fn clone(&self) -> Self {
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
    fn from(value: T) -> Self {
        RcCell::new(value)
    }
}

/// Weak counterpart of `RcCell<T>`.
#[derive(Debug)]
pub struct WkCell<T: ?Sized> {
    ptr: Weak<RefCell<T>>,
}

impl<T> WkCell<T> {
    /// Creates a `WkCell` that doesn't refer to any value.
    /// `to_rc()` will always return an `Err` for such `WkCell`s.
    pub fn new() -> Self {
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
    pub fn to_rc(&self) -> Result<RcCell<T>> {
        self.ptr.upgrade().map(|rc| RcCell { ptr: rc }).ok_or(Error::Strongify)
    }
}

/// Clones the weak pointer so that the returned result points to
/// the same value as `self` does, if any. If `self` doesn't point
/// anywhere, the clone will not be able to be strongified either.
impl<T> Clone for WkCell<T> {
    fn clone(&self) -> Self {
        WkCell {
            ptr: self.ptr.clone()
        }
    }
}

/// Creates a `WkCell` that doesn't point anywhere, just like `new()`.
///
/// (This is implemented manually because `#[derive]`ing
/// imposes the overly conservative bound `T: Default`.)
impl<T> Default for WkCell<T> {
    fn default() -> Self {
        WkCell::new()
    }
}

impl<T> Display for WkCell<T> where RcCell<T>: Display {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.to_rc() {
            Ok(rc) => rc.fmt(f),
            Err(_) => f.write_str("<cannot strongify WkCell>"),
        }
    }
}

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
    string.graphemes(true).filter(|&g| pred(g)).count()
}
