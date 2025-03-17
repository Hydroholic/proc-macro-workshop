#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use derive_debug::CustomDebug;
pub struct Field {
    name: &'static str,
    #[debug = "0b{:08b}"]
    bitmask: u8,
}
use std::fmt;
impl fmt::Debug for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Field")
            .field("name", &self.name)
            .field("bitmask", &self.bitmask)
            .finish()
    }
}
fn main() {
    let f = Field {
        name: "F",
        bitmask: 0b00011100,
    };
    let debug = ::alloc::__export::must_use({
        let res = ::alloc::fmt::format(format_args!("{0:?}", f));
        res
    });
    let expected = r#"Field { name: "F", bitmask: 0b00011100 }"#;
    match (&debug, &expected) {
        (left_val, right_val) => {
            if !(*left_val == *right_val) {
                let kind = ::core::panicking::AssertKind::Eq;
                ::core::panicking::assert_failed(
                    kind,
                    &*left_val,
                    &*right_val,
                    ::core::option::Option::None,
                );
            }
        }
    };
}
