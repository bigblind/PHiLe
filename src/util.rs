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
use error::{ DerefError, DerefResult, SyntaxResult };
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

macro_rules! hash_map {
    ($($k: expr => $v: expr),*) => {
        assoc_map!(HashMap, $($k => $v),*)
    };
    ($($k: expr => $v: expr),+,) => { hash_map!($($k => $v),+) };
}

macro_rules! btree_map {
    ($($k: expr => $v: expr),*) => {
        assoc_map!(BTreeMap, $($k => $v),*)
    };
    ($($k: expr => $v: expr),+,) => { btree_map!($($k => $v),+) };
}


#[derive(Debug)]
pub struct RcCell<T: ?Sized> {
    ptr: Rc<RefCell<T>>,
}

#[derive(Debug)]
pub struct WkCell<T: ?Sized> {
    ptr: Weak<RefCell<T>>,
}


pub fn grapheme_count(string: &str) -> usize {
    string.graphemes(true).count()
}

pub fn unescape_string_literal(string: &str) -> SyntaxResult<String> {
    if string.contains('\\') {
        unimplemented!() // TODO(H2CO3): escape string literals
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

    pub fn borrow(&self) -> DerefResult<Ref<T>> {
        self.ptr.try_borrow().map_err(DerefError::Borrow)
    }

    pub fn borrow_mut(&self) -> DerefResult<RefMut<T>> {
        self.ptr.try_borrow_mut().map_err(DerefError::BorrowMut)
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

// Tests equality based on pointer identity
impl<T> PartialEq for RcCell<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr.as_ptr() == other.ptr.as_ptr()
    }
}

impl<T> Eq for RcCell<T> {}

// Hashes the pointer address itself
impl<T> Hash for RcCell<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.ptr.as_ptr().hash(hasher)
    }
}

impl<T> WkCell<T> {
    pub fn as_rc(&self) -> DerefResult<RcCell<T>> {
        self.ptr.upgrade().map(|rc| RcCell { ptr: rc }).ok_or(DerefError::Strongify)
    }
}

impl<T> Clone for WkCell<T> {
    fn clone(&self) -> WkCell<T> {
        WkCell {
            ptr: self.ptr.clone()
        }
    }
}
