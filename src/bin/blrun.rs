use brainlove::interpreter::Interpreter;

use anyhow::{anyhow, Context, Result};
use std::env::args;
use std::fs::read;
use std::io::{stdin, stdout};

fn main() -> Result<()> {
    if args().len() != 2 {
        return Err(anyhow!(
                "Must provide a single argument with the path to a brainfuck source file: {} <source file>", args().next().unwrap()
                ));
    }
    let source: Vec<u8> = read(args().nth(1).unwrap()).context("Unable to read source file")?;
    let mut interpreter = Interpreter::new(&source, stdin(), stdout().lock())?;
    interpreter.run()?;
    Ok(())
}
