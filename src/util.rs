//
// util.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/05/2017
//

use std::rc::{ Rc, Weak };


macro_rules! hash_map(
    ($($k:expr => $v:expr),*) => ({
        let mut _tmp = ::std::collections::HashMap::new();
        $({
            let key = $k;
            let val = $v;
            _tmp.insert(key, val).map(
                |_| panic!("duplicate value for key {:#?}", key)
            );
        })*
        _tmp
    });
    ($($k:expr => $v:expr),+,) => (hash_map!($($k => $v),+))
);

pub trait ForceRc<T> {
    fn force_rc(&self) -> Rc<T>;
}

impl<T> ForceRc<T> for Weak<T> {
    fn force_rc(&self) -> Rc<T> {
        self.upgrade().expect("Rc backing Weak has been deallocated")
    }
}
