//
// util.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/05/2017
//

use std::io;
use std::error::Error;
use std::fmt::{ self, Display, Formatter };
use std::rc::{ Rc, Weak };
use std::cell::{ RefCell, Ref, RefMut, BorrowError, BorrowMutError };
use std::hash::{ Hash, Hasher };


macro_rules! hash_map {
    ($($k: expr => $v: expr),*) => ({
        let mut _tmp = ::std::collections::HashMap::new();
        $({
            let key = $k;
            let val = $v;
            _tmp.insert(key.into(), val.into()).map(
                |_| panic!("duplicate value for key {:#?}", key)
            );
        })*
        _tmp
    });
    ($($k: expr => $v: expr),+,) => (hash_map!($($k => $v),+))
}


#[derive(Debug)]
pub struct RcCell<T: ?Sized> {
    ptr: Rc<RefCell<T>>,
}

#[derive(Debug)]
pub struct WkCell<T: ?Sized> {
    ptr: Weak<RefCell<T>>,
}

#[derive(Debug)]
pub enum DerefError {
    Borrow(BorrowError),
    BorrowMut(BorrowMutError),
    Strongify,
}

#[derive(Debug)]
pub struct CustomIOError {
    pub error: io::Error,
}

pub type DerefResult<T> = Result<T, DerefError>;
pub type CustomIOResult<T> = Result<T, CustomIOError>;


impl<T> RcCell<T> {
    pub fn new(value: T) -> RcCell<T> {
        RcCell {
            ptr: Rc::new(RefCell::new(value))
        }
    }

    pub fn borrow(&self) -> DerefResult<Ref<T>> {
        self.ptr.try_borrow().map_err(|err| DerefError::Borrow(err))
    }

    pub fn borrow_mut(&self) -> DerefResult<RefMut<T>> {
        self.ptr.try_borrow_mut().map_err(|err| DerefError::BorrowMut(err))
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

impl Clone for CustomIOError {
    fn clone(&self) -> Self {
        CustomIOError {
            error: io::Error::new(self.error.kind(), self.error.description())
        }
    }
}

impl Error for CustomIOError {
    fn description(&self) -> &str {
        self.error.description()
    }

    fn cause(&self) -> Option<&Error> {
        self.error.cause()
    }
}

impl Display for CustomIOError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.error.fmt(f)
    }
}

impl From<io::Error> for CustomIOError {
    fn from(error: io::Error) -> Self {
        CustomIOError { error }
    }
}

impl From<CustomIOError> for io::Error {
    fn from(error: CustomIOError) -> Self {
        error.error
    }
}
