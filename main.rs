// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

// use derive_debug::CustomDebug;
// use std::fmt::Debug;

// pub trait Trait {
//     type Value;
// }

// #[derive(CustomDebug)]
// pub struct Field<T: Trait> {
//     values: Vec<T::Value>,
// }

// fn assert_debug<F: Debug>() {}

// fn main() {
//     // Does not implement Debug, but its associated type does.
//     struct Id;

//     impl Trait for Id {
//         type Value = u8;
//     }

//     assert_debug::<Field<Id>>();
// }

//#![feature(prelude_import)]
//#[prelude_import]
//use std::prelude::rust_2021::*;
//#[macro_use]
//extern crate std;
//use derive_debug::CustomDebug;
//use std::fmt::Debug;
//pub trait Trait {
//    type Value;
//}
//pub struct Field<T: Trait> {
//    values: Vec<T::Value>,
//}
//impl<T: Trait> std::fmt::Debug for Field<T>
//where
//    T::Value: Debug,
//{
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        f.debug_struct("Field")
//            .field("values", &self.values)
//            .finish()
//    }
//}
//fn assert_debug<F: Debug>() {}
//fn main() {
//    struct Id;
//    impl Trait for Id {
//        type Value = u8;
//    }
//    assert_debug::<Field<Id>>();
//}

use derive_debug::CustomDebug;
use std::fmt::Debug;

pub trait Trait {
    type Value;
}

#[derive(CustomDebug)]
pub struct Field<T: Trait> {
    values: Vec<T::Value>,
}

fn assert_debug<F: Debug>() {}

fn main() {
    // Does not implement Debug, but its associated type does.
    struct Id;

    impl Trait for Id {
        type Value = u8;
    }

    assert_debug::<Field<Id>>();
}
