use anyhow::{anyhow, Result};
use std::collections::HashSet;
use std::io::{Read, Write};

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
pub struct Interpreter<Input: Read, Output: Write> {
    code: Vec<Instr>,
    input: Input,
    output: Output,
    ip: usize,
    ptr: usize,
    memory: [u8; MEMORY_SIZE],
    breakpoints: HashSet<usize>,
}

fn generate_code(source: &[u8]) -> Result<Vec<Instr>> {
    let mut code: Vec<Instr> = source
        .iter()
        .filter_map(|c| match c {
            b'>' => Some(Instr::Next),
            b'<' => Some(Instr::Prev),
            b'+' => Some(Instr::Incr),
            b'-' => Some(Instr::Decr),
            b'.' => Some(Instr::Output),
            b',' => Some(Instr::Input),
            b'[' => Some(Instr::While(0)),
            b']' => Some(Instr::End(0)),
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

impl<Input: Read, Output: Write> Interpreter<Input, Output> {
    pub fn new(source: &[u8], input: Input, output: Output) -> Result<Self> {
        Ok(Self {
            code: generate_code(source)?,
            input,
            output,
            ip: Default::default(),
            ptr: Default::default(),
            memory: [0; MEMORY_SIZE],
            breakpoints: Default::default(),
        })
    }

    pub fn run(&mut self) -> Result<()> {
        while self.ip < self.code.len() {
            self.step()?;
            if self.breakpoints.contains(&self.ip) {
                break;
            }
        }
        Ok(())
    }

    pub fn step(&mut self) -> Result<()> {
        match self.code[self.ip] {
            Instr::Next => self.ptr += 1,
            Instr::Prev => self.ptr -= 1,
            Instr::Incr => *self.data() = self.data().wrapping_add(1),
            Instr::Decr => *self.data() = self.data().wrapping_sub(1),
            Instr::Output => {
                let datum = *self.data();
                self.output.write_all(&[datum])?;
            }
            Instr::Input => {
                let mut buf: [u8; 1] = [0; 1];
                let char = match self.input.read(&mut buf[..])? {
                    1 => buf[0],
                    _ => 255,
                };
                *self.data() = char;
            }
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
        Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hello_world() -> Result<()> {
        let source = br#"
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
        let input = b"";
        let mut output = vec![];
        let mut interpreter = Interpreter::new(source, &input[..], &mut output)?;
        interpreter.run()?;
        assert_eq!(output, b"Hello World!\n");
        Ok(())
    }

    #[test]
    fn test_rot13() -> Result<()> {
        let source = br#"
-,+[                         Read first character and start outer character reading loop
    -[                       Skip forward if character is 0
        >>++++[>++++++++<-]  Set up divisor (32) for division loop
                               (MEMORY LAYOUT: dividend copy remainder divisor quotient zero zero)
        <+<-[                Set up dividend (x minus 1) and enter division loop
            >+>+>-[>>>]      Increase copy and remainder / reduce divisor / Normal case: skip forward
            <[[>+<-]>>+>]    Special case: move remainder back to divisor and increase quotient
            <<<<<-           Decrement dividend
        ]                    End division loop
    ]>>>[-]+                 End skip loop; zero former divisor and reuse space for a flag
    >--[-[<->+++[-]]]<[         Zero that flag unless quotient was 2 or 3; zero quotient; check flag
        ++++++++++++<[       If flag then set up divisor (13) for second division loop
                               (MEMORY LAYOUT: zero copy dividend divisor remainder quotient zero zero)
            >-[>+>>]         Reduce divisor; Normal case: increase remainder
            >[+[<+>-]>+>>]   Special case: increase remainder / move it back to divisor / increase quotient
            <<<<<-           Decrease dividend
        ]                    End division loop
        >>[<+>-]             Add remainder back to divisor to get a useful 13
        >[                   Skip forward if quotient was 0
            -[               Decrement quotient and skip forward if quotient was 1
                -<<[-]>>     Zero quotient and divisor if quotient was 2
            ]<<[<<->>-]>>    Zero divisor and subtract 13 from copy if quotient was 1
        ]<<[<<+>>-]          Zero divisor and add 13 to copy if quotient was 0
    ]                        End outer skip loop (jump to here if ((character minus 1)/32) was not 2 or 3)
    <[-]                     Clear remainder from first division if second division was skipped
    <.[-]                    Output ROT13ed character from copy and clear it
    <-,+                     Read next character
]                            End character reading loop
"#;
        let input = b"Hello, world!";
        let mut output = vec![];
        let mut interpreter = Interpreter::new(source, &input[..], &mut output)?;
        interpreter.run()?;
        assert_eq!(output, b"Uryyb, jbeyq!");
        Ok(())
    }
}
