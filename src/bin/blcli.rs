use brainlove::interpreter::Interpreter;

use anyhow::{anyhow, Context, Result};
use std::env::args;
use std::fmt::Display;
use std::fs::read;
use std::io::{stdin, Read, Write};
use std::str::FromStr;

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

enum Command {
    NoOp,
    Step,
    Break(Option<usize>),
    Unbreak(Option<usize>),
    Run,
    Print,
    Help,
}

impl FromStr for Command {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = s.trim().split(' ').collect::<Vec<_>>();
        if tokens.is_empty() {
            return Ok(Command::NoOp);
        }
        match tokens[0] {
            "step" => {
                if tokens.len() > 1 {
                    Err(anyhow!("\"step\" does not take any additional information"))
                } else {
                    Ok(Command::Step)
                }
            }
            "break" => {
                if tokens.len() == 1 {
                    Ok(Command::Break(None))
                } else if tokens.len() == 2 {
                    tokens[1].parse::<usize>().map_or_else(
                        |_| Err(anyhow!("please use a valid integer for \"break\" offset")),
                        |result| Ok(Command::Break(Some(result))),
                    )
                } else {
                    Err(anyhow!("\"break\" takes a single optional integer offset"))
                }
            }
            "unbreak" => {
                if tokens.len() == 1 {
                    Ok(Command::Unbreak(None))
                } else if tokens.len() == 2 {
                    tokens[1].parse::<usize>().map_or_else(
                        |_| Err(anyhow!("please use a valid integer for \"unbreak\" offset")),
                        |result| Ok(Command::Unbreak(Some(result))),
                    )
                } else {
                    Err(anyhow!("\"break\" takes a single optional integer offset"))
                }
            }
            "run" => {
                if tokens.len() > 1 {
                    Err(anyhow!("\"run\" does not take any additional information"))
                } else {
                    Ok(Command::Run)
                }
            }
            "print" => {
                if tokens.len() > 1 {
                    Err(anyhow!(
                        "\"print\" does not take any additional information"
                    ))
                } else {
                    Ok(Command::Print)
                }
            }
            "help" => {
                if tokens.len() > 1 {
                    Err(anyhow!("\"help\" does not take any additional information"))
                } else {
                    Ok(Command::Help)
                }
            }
            unknown_command => Err(anyhow!("Unknown command \"{}\"", unknown_command)),
        }
    }
}

#[derive(Debug)]
struct LineHandler<Input: Read, Output: Write + Display>(Interpreter<Input, Output>);

impl<Input: Read, Output: Write + Display> LineHandler<Input, Output> {
    fn handle_line(&mut self, line: String) -> Result<()> {
        match line.parse()? {
            Command::NoOp => Ok(()),
            Command::Step => self.0.step(),
            Command::Break(offset) => self.0.add_breakpoint(offset),
            Command::Unbreak(offset) => {
                self.0.remove_breakpoint(offset);
                Ok(())
            }
            Command::Run => self.0.run(),
            Command::Print => {
                println!("{}", self.0);
                Ok(())
            }
            Command::Help => {
                print!(
                    r#"The available commands are:
  - step: Take 1 step in the interpreter.
  - run: Run until the next breakpoint or the end of the program.
  - break (<offset>): Set a breakpoint at the provided offset or the 
    current instruction pointer if no offset is provided.
  - unbreak (<offset>): Unset the breakpoint at the provided offset or the
    current instruction pointer if no offset is provided.
  - print: Print the current state of the interpreter.
  - help: Display this help text.
"#
                );
                Ok(())
            }
        }
    }
}

fn main() -> Result<()> {
    if args().len() != 2 {
        return Err(anyhow!(
                "Must provide a single argument with the path to a brainfuck source file: {} <source file>", args().next().unwrap()
                ));
    }
    let source: Vec<u8> = read(args().nth(1).unwrap()).context("Unable to read source file")?;
    let mut line_handler = {
        let interpreter = Interpreter::new(&source, stdin(), BrainloveOutput::default())?;
        LineHandler(interpreter)
    };
    let mut rl = rustyline::Editor::<()>::new()?;
    for readline in rl.iter("> ") {
        match readline {
            Ok(line) => {
                if let Err(err) = line_handler.handle_line(line) {
                    println!("Error handling line: {:?}", err);
                }
            }
            Err(err) => {
                println!("Unrecoverable error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
