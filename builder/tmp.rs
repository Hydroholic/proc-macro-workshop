#![feature(prelude_import)]
#[prelude_import]
use std::prelude::rust_2021::*;
#[macro_use]
extern crate std;
use derive_builder::Builder;
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: Option<String>,
}
use std::error::Error;
pub struct CommandBuilder {
    executable: Option<String>,
    args: Option<Vec<String>>,
    env: Option<Vec<String>>,
    current_dir: Option<String>,
}
impl CommandBuilder {
    fn executable(&mut self, value: String) -> &mut Self {
        self.executable = Some(value);
        self
    }
    fn args(&mut self, value: Vec<String>) -> &mut Self {
        self.args = Some(value);
        self
    }
    fn env(&mut self, value: Vec<String>) -> &mut Self {
        self.env = Some(value);
        self
    }
    fn current_dir(&mut self, value: String) -> &mut Self {
        self.current_dir = Some(value);
        self
    }
    pub fn build(&mut self) -> Result<Command, Box<dyn Error>> {
        if let CommandBuilder {
            executable: Some(executable),
            args: Some(args),
            env: Some(env),
            current_dir,
        } = self {
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
            args: None,
            env: None,
            current_dir: None,
        }
    }
}
fn main() {}
