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


// Generic macro for building an associated container literal
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

macro_rules! btree_map {
    ($($k: expr => $v: expr),*) => {
        assoc_map!(BTreeMap, $($k => $v),*)
    };
    ($($k: expr => $v: expr),+,) => { btree_map!($($k => $v),+) };
}

#[derive(Debug, Clone, Copy)]
pub struct PackageInfo {
    pub name:        &'static str,
    pub version:     &'static str,
    pub authors:     &'static str,
    pub description: &'static str,
    pub home_page:   &'static str,
}

#[derive(Debug, Clone, Copy)]
pub struct Color {
    pub reset:     &'static str,
    pub info:      &'static str,
    pub highlight: &'static str,
    pub success:   &'static str,
    pub error:     &'static str,
}

#[derive(Debug, Default)]
pub struct RcCell<T: ?Sized> {
    ptr: Rc<RefCell<T>>,
}

#[derive(Debug, Default)]
pub struct WkCell<T: ?Sized> {
    ptr: Weak<RefCell<T>>,
}

pub static PACKAGE_INFO: PackageInfo = PackageInfo {
    name:        env!["CARGO_PKG_NAME"],
    version:     env!["CARGO_PKG_VERSION"],
    authors:     env!["CARGO_PKG_AUTHORS"],
    description: env!["CARGO_PKG_DESCRIPTION"],
    home_page:   env!["CARGO_PKG_HOMEPAGE"],
};

pub static COLOR: Color = Color {
    reset:     "\x1b[0m",
    info:      "\x1b[1;33m",
    highlight: "\x1b[1;36m",
    success:   "\x1b[1;32m",
    error:     "\x1b[1;31m",
};


pub fn grapheme_count(string: &str) -> usize {
    string.graphemes(true).count()
}

pub fn grapheme_count_by<P: Fn(&str) -> bool>(string: &str, pred: P) -> usize {
    string.graphemes(true).filter(|g| pred(*g)).count()
}

pub fn unescape_string_literal(string: &str) -> Result<String> {
    if string.contains('\\') {
        unimplemented!() // TODO(H2CO3): unescape string literals
    } else {
        Ok(string.to_owned())
    }
}


impl<T> RcCell<T> {
    pub fn new(value: T) -> RcCell<T> {
        RcCell {
            ptr: Rc::new(RefCell::new(value))
        }
    }

    pub fn borrow(&self) -> Result<Ref<T>> {
        self.ptr.try_borrow().map_err(From::from)
    }

    pub fn borrow_mut(&self) -> Result<RefMut<T>> {
        self.ptr.try_borrow_mut().map_err(From::from)
    }

    pub fn as_weak(&self) -> WkCell<T> {
        WkCell {
            ptr: Rc::downgrade(&self.ptr)
        }
    }
}

impl<T> Clone for RcCell<T> {
    fn clone(&self) -> RcCell<T> {
        RcCell {
            ptr: self.ptr.clone()
        }
    }
}

// TODO(H2CO3): implement this once { Unsize, CoerceUnsized } are stable
// impl<T: ?Sized + Unsize<U>, U: ?Sized> CoerceUnsized<RcCell<U>> for RcCell<T> {}

/// Tests equality based on pointer identity
impl<T> PartialEq for RcCell<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.ptr, &other.ptr)
    }
}

/// Tests equality based on pointer identity
impl<T> Eq for RcCell<T> {}

/// Hashes the pointer address itself
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

impl<T> WkCell<T> {
    pub fn new() -> WkCell<T> {
        WkCell {
            ptr: Weak::new()
        }
    }

    pub fn as_rc(&self) -> Result<RcCell<T>> {
        self.ptr.upgrade().map(|rc| RcCell { ptr: rc }).ok_or(Error::Strongify)
    }
}

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
