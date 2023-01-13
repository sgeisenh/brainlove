use brainlove::interpreter::Interpreter;

use anyhow::{anyhow, Context, Result};
use std::env::args;
use std::fmt::Display;
use std::fs::read;
use std::io::{stdin, Write};

#[derive(Default)]
struct BrainloveOutput(Vec<u8>);

impl Display for BrainloveOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        String::from_utf8_lossy(&self.0).fmt(f)
    }
}

impl Write for BrainloveOutput {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

fn main() -> Result<()> {
    if args().len() != 2 {
        return Err(anyhow!(
                "Must provide a single argument with the path to a brainfuck source file: {} <source file>", args().next().unwrap()
                ));
    }
    let source: Vec<u8> = read(args().nth(1).unwrap()).context("Unable to read source file")?;
    let mut interpreter = Interpreter::new(&source, stdin(), BrainloveOutput::default())?;
    println!("{}", &interpreter);
    while let Ok(_) = interpreter.step() {
        println!("{}", &interpreter);
    }
    Ok(())
}
