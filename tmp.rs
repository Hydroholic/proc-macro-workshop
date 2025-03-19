#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use derive_debug::CustomDebug;
pub struct Field<T> {
    value: T,
    #[debug = "0b{:08b}"]
    bitmask: u8,
}
impl<T: std::fmt::Debug> std::fmt::Debug for Field<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Field")
            .field("value", &self.value)
            .field("bitmask", &format_args!("0b{0:08b}", &self.bitmask))
            .finish()
    }
}
fn main() {
    let f = Field {
        value: "F",
        bitmask: 0b00011100,
    };
    let debug = ::alloc::__export::must_use({
        let res = ::alloc::fmt::format(format_args!("{0:?}", f));
        res
    });
    let expected = r#"Field { value: "F", bitmask: 0b00011100 }"#;
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
