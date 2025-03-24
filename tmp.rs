#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use derive_debug::CustomDebug;
use std::fmt::Debug;
pub trait Trait {
    type Value;
}
pub struct Field<T: Trait> {
    values: Vec<T::Value>,
}
impl<T: Trait + std::fmt::Debug> std::fmt::Debug for Field<T::Value> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Field").field("values", &self.values).finish()
    }
}
fn assert_debug<F: Debug>() {}
fn main() {
    struct Id;
    impl Trait for Id {
        type Value = u8;
    }
    assert_debug::<Field<Id>>();
}
