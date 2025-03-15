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
pub struct CommandBuilder {
    executable: Option<String>,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: Option<String>,
}
impl CommandBuilder {
    fn executable(&mut self, value: String) -> &mut Self {
        self.executable = Some(value);
        self
    }
    fn args(&mut self, value: Vec<String>) -> &mut Self {
        self.args = value;
        self
    }
    fn current_dir(&mut self, value: String) -> &mut Self {
        self.current_dir = Some(value);
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
    pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
        if let CommandBuilder { executable: Some(executable), args, env, current_dir } = self {
            Ok(Command {
                executable: executable.to_owned(),
                args: args.to_owned(),
                env: env.to_owned(),
                current_dir: current_dir.to_owned(),
            })
        } else {
            Err(Box::from("Could not build because of missing attributes."))
        }
    }
}
impl Command {
    pub fn builder() -> CommandBuilder {
        CommandBuilder {
            executable: None,
            args: Vec::new(),
            env: Vec::new(),
            current_dir: None,
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
