#![allow(unused_variables, dead_code)]

use anyhow::{anyhow, Result};
use std::collections::HashSet;

#[derive(Debug)]
enum Instr {
    Next,         // >
    Prev,         // <
    Incr,         // +
    Decr,         // -
    Output,       // .
    Input,        // ,
    While(usize), // [ offset to false case
    End(usize),   // ] offset back to opening [
}

const MEMORY_SIZE: usize = 30000;

#[derive(Debug)]
struct Interpreter {
    output: Vec<u8>,
    code: Vec<Instr>,
    ip: usize,
    ptr: usize,
    memory: [u8; MEMORY_SIZE],
    breakpoints: HashSet<usize>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            output: Default::default(),
            code: Default::default(),
            ip: Default::default(),
            ptr: Default::default(),
            memory: [0; MEMORY_SIZE],
            breakpoints: Default::default(),
        }
    }
}

fn generate_code(source: &str) -> Result<Vec<Instr>> {
    let mut code: Vec<Instr> = source
        .chars()
        .filter_map(|c| match c {
            '>' => Some(Instr::Next),
            '<' => Some(Instr::Prev),
            '+' => Some(Instr::Incr),
            '-' => Some(Instr::Decr),
            '.' => Some(Instr::Output),
            ',' => Some(Instr::Input),
            '[' => Some(Instr::While(0)),
            ']' => Some(Instr::End(0)),
            _ => None,
        })
        .collect();
    let mut stack: Vec<(&mut usize, usize)> = vec![];
    for (offset, instr) in code.iter_mut().enumerate() {
        match instr {
            Instr::While(start_ref) => {
                stack.push((start_ref, offset));
            }
            Instr::End(end_ref) => {
                let (start_ref, start_offset) = match stack.pop() {
                    Some(entry) => entry,
                    None => return Err(anyhow!("Mismatched ']'.")),
                };
                *start_ref = offset;
                *end_ref = start_offset;
            }
            _ => (),
        }
    }
    if !stack.is_empty() {
        return Err(anyhow!("Mismatched '['."));
    }
    Ok(code)
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get_output(&self) -> &[u8] {
        &self.output
    }

    pub fn run(&mut self, source: &str) -> Result<()> {
        self.code = generate_code(source)?;
        println!("{:?}", self.code);
        while self.ip < self.code.len() {
            self.step();
            if self.breakpoints.contains(&self.ip) {
                break;
            }
        }
        Ok(())
    }

    pub fn step(&mut self) {
        match self.code[self.ip] {
            Instr::Next => self.ptr += 1,
            Instr::Prev => self.ptr -= 1,
            Instr::Incr => *self.data() = self.data().wrapping_add(1),
            Instr::Decr => *self.data() = self.data().wrapping_sub(1),
            Instr::Output => {
                let datum = *self.data();
                self.output.push(datum);
            }
            Instr::Input => todo!(),
            Instr::While(offset) => {
                if *self.data() == 0 {
                    self.ip = offset
                }
            }
            Instr::End(offset) => {
                if *self.data() != 0 {
                    self.ip = offset
                }
            }
        }
        self.ip += 1;
    }

    fn data(&mut self) -> &mut u8 {
        &mut self.memory[self.ptr]
    }

    pub fn add_breakpoint(&mut self, offset: usize) -> Result<()> {
        if offset >= self.code.len() {
            return Err(anyhow!("Offset is out of bounds."));
        }
        self.breakpoints.insert(offset);
        Ok(())
    }

    pub fn remove_breakpoint(&mut self, offset: usize) {
        self.breakpoints.remove(&offset);
    }
}

fn main() -> Result<()> {
    let source = r#"
[ This program prints "Hello World!" and a newline to the screen, its
  length is 106 active command characters. [It is not the shortest.]

  This loop is an "initial comment loop", a simple way of adding a comment
  to a BF program such that you don't have to worry about any command
  characters. Any ".", ",", "+", "-", "<" and ">" characters are simply
  ignored, the "[" and "]" characters just have to be balanced. This
  loop and the commands it contains are ignored because the current cell
  defaults to a value of 0; the 0 value causes this loop to be skipped.
]
++++++++               Set Cell #0 to 8
[
    >++++               Add 4 to Cell #1; this will always set Cell #1 to 4
    [                   as the cell will be cleared by the loop
        >++             Add 2 to Cell #2
        >+++            Add 3 to Cell #3
        >+++            Add 3 to Cell #4
        >+              Add 1 to Cell #5
        <<<<-           Decrement the loop counter in Cell #1
    ]                   Loop until Cell #1 is zero; number of iterations is 4
    >+                  Add 1 to Cell #2
    >+                  Add 1 to Cell #3
    >-                  Subtract 1 from Cell #4
    >>+                 Add 1 to Cell #6
    [<]                 Move back to the first zero cell you find; this will
                        be Cell #1 which was cleared by the previous loop
    <-                  Decrement the loop Counter in Cell #0
]                       Loop until Cell #0 is zero; number of iterations is 8

The result of this is:
Cell no :   0   1   2   3   4   5   6
Contents:   0   0  72 104  88  32   8
Pointer :   ^

>>.                     Cell #2 has value 72 which is 'H'
>---.                   Subtract 3 from Cell #3 to get 101 which is 'e'
+++++++..+++.           Likewise for 'llo' from Cell #3
>>.                     Cell #5 is 32 for the space
<-.                     Subtract 1 from Cell #4 for 87 to give a 'W'
<.                      Cell #3 was set to 'o' from the end of 'Hello'
+++.------.--------.    Cell #3 for 'rl' and 'd'
>>+.                    Add 1 to Cell #5 gives us an exclamation point
>++.                    And finally a newline from Cell #6
        "#;
    let mut interpreter = Interpreter::new();
    interpreter.run(source)?;
    println!("Output: {:?}", std::str::from_utf8(interpreter.get_output())?);
    Ok(())
}
