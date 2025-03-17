#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use derive_builder::Builder;
pub struct Command {
    executable: String,
    #[builder(each = "arg")]
    args: Vec<String>,
    #[builder(each = "env")]
    env: Vec<String>,
    current_dir: Option<String>,
}
use std::error::Error;
fn test() {
    let a = "String";
    let a = "Vec";
    let a = "Vec";
    let a = "Option";
}
pub struct CommandBuilder {
    executable: Option<String>,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: Option<String>,
}
impl CommandBuilder {
    fn executable(&mut self, value: String) -> &mut Self {
        self.executable = std::option::Option::Some(value);
        self
    }
    fn args(&mut self, value: Vec<String>) -> &mut Self {
        self.args = value;
        self
    }
    fn current_dir(&mut self, value: String) -> &mut Self {
        self.current_dir = std::option::Option::Some(value);
        self
    }
    fn arg(&mut self, value: String) -> &mut Self {
        self.args.push(value);
        self
    }
    fn env(&mut self, value: String) -> &mut Self {
        self.env.push(value);
        self
    }
    pub fn build(&mut self) -> std::result::Result<Command, std::boxed::Box<dyn Error>> {
        if let CommandBuilder {
            executable: std::option::Option::Some(executable),
            args,
            env,
            current_dir,
        } = self {
            std::result::Result::Ok(Command {
                executable: executable.to_owned(),
                args: args.to_owned(),
                env: env.to_owned(),
                current_dir: current_dir.to_owned(),
            })
        } else {
            std::result::Result::Err(
                std::boxed::Box::from("Could not build because of missing attributes."),
            )
        }
    }
}
impl Command {
    pub fn builder() -> CommandBuilder {
        CommandBuilder {
            executable: std::option::Option::None,
            args: std::vec::Vec::new(),
            env: std::vec::Vec::new(),
            current_dir: std::option::Option::None,
        }
    }
}
fn main() {
    let command = Command::builder()
        .executable("cargo".to_owned())
        .arg("build".to_owned())
        .arg("--release".to_owned())
        .build()
        .unwrap();
    match (&command.executable, &"cargo") {
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
    match (
        &command.args,
        &<[_]>::into_vec(::alloc::boxed::box_new(["build", "--release"])),
    ) {
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
