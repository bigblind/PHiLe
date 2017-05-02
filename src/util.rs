//
// util.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 02/05/2017
//

macro_rules! hash_map(
    ($($k:expr => $v:expr),*) => ({
        let mut _tmp = ::std::collections::HashMap::new();
        $({
            let key = $k;
            let val = $v;
            _tmp.insert(key, val).map(
                |_| panic!("duplicate value {:#?} for key {:#?}", val, key)
            );
        })*
        _tmp
    });
    ($($k:expr => $v:expr),+,) => (hash_map!($($k => $v),+))
);
