use anyhow::Result;
use interpreter::Interpreter;
use std::{
    cell::{Cell, RefCell},
    collections::HashSet,
    fmt::Display,
    io::{Read, Write},
    rc::Rc,
};
use stylist::yew::use_style;
use web_sys::HtmlTextAreaElement;
use yew::{prelude::*, virtual_dom::VNode};

#[derive(Properties, PartialEq)]
pub struct InstructionProps {
    instruction: String,
    current: bool,
    brk: bool,
    offset: usize,
    onclick: Callback<usize>,
}

#[function_component]
fn Instruction(props: &InstructionProps) -> Html {
    let InstructionProps {
        instruction,
        current,
        brk,
        offset,
        onclick,
    } = props;
    let offset = *offset;
    let onclick = {
        let onclick = onclick.clone();
        move |_| {
            onclick.emit(offset);
        }
    };
    let current_style = current.then_some(use_style!("color: red;"));
    let brk_style = brk.then_some(use_style!("text-decoration: green wavy underline;"));
    let class = classes!(current_style, brk_style);
    html! {
        <span
            {class}
            {onclick}
        >
            {instruction}
        </span>
    }
}

#[derive(Properties, PartialEq)]
pub struct CodeProps {
    code: Vec<interpreter::Instr>,
    breakpoints: HashSet<usize>,
    onclick: Callback<usize>,
    ip: usize,
}

#[function_component]
fn Code(props: &CodeProps) -> Html {
    let CodeProps {
        code,
        breakpoints,
        onclick,
        ip,
    } = props;
    let create_instruction = |(idx, instr): (usize, &interpreter::Instr)| {
        let instruction = instr.to_string();
        let current = idx == *ip;
        let brk = breakpoints.contains(&idx);
        let offset = idx;
        html! {
            <Instruction {instruction} {current} {brk} {offset} {onclick}/>
        }
    };
    html! {
        <>
            <h2>{ "Processed Source" }</h2>
            <p>{"Click on an instruction to place or remove a breakpoint!"}</p>
            <code>
                { for code.iter().enumerate().map(create_instruction) }
            </code>
        </>
    }
}

#[derive(Properties, PartialEq, Clone)]
struct LineProps {
    line_offset: usize,
    byte_chunks: Vec<ByteChunkProps>,
}

#[function_component]
fn Line(props: &LineProps) -> Html {
    let prefix = format!("{:08x?} ", props.line_offset);
    let render_chunk = |chunk_props: &ByteChunkProps| {
        html! {
            <ByteChunk ..chunk_props.clone() />
        }
    };
    html! {
        <p>
            {prefix}
            { for props.byte_chunks.iter().map(render_chunk) }
        </p>
    }
}

#[derive(Properties, PartialEq, Clone)]
struct ByteChunkProps {
    bl_bytes: Vec<BlByteProps>,
}

#[function_component]
fn ByteChunk(props: &ByteChunkProps) -> Html {
    let class = use_style!("padding-left: 1em; padding-right: 1em;");
    let render_byte = |byte_props: &BlByteProps| {
        html! {
            <BlByte ..byte_props.clone() />
        }
    };
    html! {
        <span {class}>
            { for props.bl_bytes.iter().map(render_byte) }
        </span>
    }
}

#[derive(Properties, PartialEq, Clone)]
struct BlByteProps {
    highlighted: bool,
    byte: u8,
}

#[function_component]
fn BlByte(props: &BlByteProps) -> Html {
    let BlByteProps { highlighted, byte } = props;
    let highlighted_style = highlighted.then_some(use_style!("color: orange; font-weight: bold;"));
    let class = classes!(highlighted_style);
    html! {
        <span {class}>{format!("{:02x?}", byte)}</span>
    }
}

#[derive(Properties, PartialEq, Eq)]
pub struct MemoryProps {
    contents: Vec<u8>,
    dp: usize,
}

#[function_component]
fn Memory(props: &MemoryProps) -> Html {
    let MemoryProps { contents, dp } = props;
    let mut lines: Vec<VNode> = vec![];
    let mut previous = 0;
    // We want to group the memory into chunks of 16 bytes, each of which have
    // chunks of 2 bytes each.

    // A line with at most 8 chunks of at most 2 bytes a piece.

    // A single byte to display.

    for (line_offset, line) in contents
        .chunks(16)
        .enumerate()
        .filter(|(chunk_idx, chunk)| {
            *chunk_idx == 0
                || (chunk_idx * 16..(chunk_idx + 1) * 16).contains(dp)
                || !chunk.iter().all(|byte| *byte == 0)
        })
        .map(|(chunk_idx, chunk)| {
            let line_offset = chunk_idx * 16;
            let byte_chunks = chunk
                .chunks(2)
                .enumerate()
                .map(|(pair_idx, col)| {
                    let bl_bytes = col
                        .iter()
                        .enumerate()
                        .map(|(inner_idx, &byte)| {
                            let highlighted = chunk_idx * 16 + pair_idx * 2 + inner_idx == *dp;
                            BlByteProps { highlighted, byte }
                        })
                        .collect::<Vec<_>>();
                    ByteChunkProps { bl_bytes }
                })
                .collect::<Vec<_>>();
            (
                line_offset,
                html! {
                    <Line {line_offset} {byte_chunks} />
                },
            )
        })
    {
        if line_offset - previous > 16 {
            let ellipsis_node = html! {
                <p>{ "..." }</p>
            };
            lines.push(ellipsis_node);
        }
        lines.push(line);
        previous = line_offset;
    }
    let class = use_style!("font-family: monospace;");
    html! {
        <>
            <h2>{ "Memory" }</h2>
            <div {class}>
                { lines }
            </div>
        </>
    }
}

pub enum InterpMsg {
    Step,
    ToggleBreak(usize),
    Run,
}

#[derive(Clone)]
pub struct WebInput {
    input: Rc<RefCell<String>>,
    pos: Cell<usize>,
}

impl WebInput {
    fn new<S: Into<String>>(input: S) -> Self {
        Self {
            input: Rc::new(RefCell::new(input.into())),
            pos: Cell::new(0),
        }
    }

    fn set(&self, input: String) {
        *self.input.borrow_mut() = input;
    }

    fn reset_pos(&self) {
        self.pos.set(0);
    }
}

impl Read for WebInput {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let inner = self.input.borrow();
        if self.pos.get() >= inner.len() {
            return Ok(0);
        }
        (&inner.as_bytes()[self.pos.get()..])
            .read(buf)
            .map(|num_bytes| {
                self.pos.set(self.pos.get() + num_bytes);
                num_bytes
            })
    }
}

type WebInterpreter = Rc<RefCell<Interpreter<WebInput, WebOutput>>>;

#[derive(Properties)]
pub struct InterpProps {
    interpreter: WebInterpreter,
}

impl PartialEq for InterpProps {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.interpreter, &other.interpreter)
    }
}

pub struct InterpComp(Result<()>);

#[derive(Default)]
pub struct WebOutput(Vec<u8>);

impl Display for WebOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}

impl Write for WebOutput {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

#[derive(Properties, PartialEq)]
struct OutputProps {
    output: String,
}

#[function_component]
fn Output(props: &OutputProps) -> Html {
    let class = use_style!("width: 55em; height: 55em;");
    html! {
        <>
            <h2>{ "Output" }</h2>
            <textarea {class} readonly={true} value={props.output.clone()}></textarea>
        </>
    }
}

impl Component for InterpComp {
    type Message = InterpMsg;

    type Properties = InterpProps;

    fn create(_ctx: &Context<Self>) -> Self {
        Self(Ok(()))
    }

    fn update(&mut self, ctx: &Context<Self>, msg: Self::Message) -> bool {
        // If we have an error, ignore any messages.
        if self.0.is_err() {
            return false;
        }

        let mut interpreter = ctx.props().interpreter.borrow_mut();
        match msg {
            InterpMsg::Step => {
                self.0 = interpreter.step();
                true
            }
            InterpMsg::ToggleBreak(offset) => {
                if interpreter.get_breakpoints().contains(&offset) {
                    interpreter.remove_breakpoint(Some(offset));
                } else {
                    self.0 = interpreter.add_breakpoint(Some(offset));
                }
                true
            }
            InterpMsg::Run => {
                self.0 = interpreter.run();
                true
            }
        }
    }

    fn view(&self, ctx: &Context<Self>) -> Html {
        let interpreter = &ctx.props().interpreter;
        let code_markup = {
            let interpreter = interpreter.borrow();
            let code = interpreter.get_code().to_vec();
            let breakpoints = interpreter.get_breakpoints().clone();
            let onclick = ctx.link().callback(InterpMsg::ToggleBreak);
            let ip = interpreter.get_ip();
            html! {
                <Code {code}
                    {breakpoints}
                    {onclick}
                    {ip} />
            }
        };
        let memory_markup = {
            let interpreter = interpreter.borrow();
            let contents = interpreter.get_memory().to_vec();
            let dp = interpreter.get_ptr();
            html! {
                <Memory {contents} {dp} />
            }
        };
        let output_markup = {
            let interpreter = interpreter.borrow();
            let output = interpreter.get_output().to_string();
            html! {
                <Output {output} />
            }
        };
        html! {
            <>
                <div>
                    <button onclick={ctx.link().callback(|_| InterpMsg::Step)}>
                        { "step" }
                    </button>
                    <button onclick={ctx.link().callback(|_| InterpMsg::Run)}>
                        { "run" }
                    </button>
                </div>
                {code_markup}
                {memory_markup}
                {output_markup}
            </>
        }
    }
}

const HELLO_WORLD: &str = r#"[ This program prints "Hello World!" and a newline to the screen, its
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

#[function_component]
fn App() -> Html {
    let source_node_ref = use_node_ref();
    let input_node_ref = use_node_ref();

    let input_value_handle = use_state(|| "".to_string());
    let input_value = (*input_value_handle).clone();
    let source_value_handle = use_state(|| HELLO_WORLD.to_string());
    let source_value = (*source_value_handle).clone();
    let web_input_handle = use_state(|| WebInput::new(""));
    let web_input_value = (*web_input_handle).clone();
    let interpreter_handle = use_state({
        || -> Result<WebInterpreter> {
            Ok(Rc::new(RefCell::new(Interpreter::new(
                &source_value.bytes().collect::<Vec<_>>(),
                WebInput::new(
                    input_node_ref
                        .cast::<HtmlTextAreaElement>()
                        .map_or("".into(), |v| v.value()),
                ),
                WebOutput::default(),
            )?)))
        }
    });
    let interpreter_value = &*interpreter_handle;
    let interpreter_markup = match interpreter_value {
        Ok(interpreter) => {
            html! {
                <InterpComp {interpreter}/>
            }
        }
        Err(e) => {
            html! {
                {e}
            }
        }
    };

    let onsourceinput = {
        let source_node_ref = source_node_ref.clone();
        let web_input = web_input_value.clone();

        Callback::from(move |_| {
            let source = source_node_ref.cast::<HtmlTextAreaElement>();

            if let Some(source) = source {
                source_value_handle.set(source.value());
                web_input.reset_pos();
                interpreter_handle.set(
                    Interpreter::new(
                        &source.value().into_bytes(),
                        web_input.clone(),
                        WebOutput::default(),
                    )
                    .map(|interpreter| Rc::new(RefCell::new(interpreter))),
                );
            }
        })
    };

    let oninputinput = {
        let input_node_ref = input_node_ref.clone();
        let web_input = web_input_value;
        Callback::from(move |_| {
            let input = input_node_ref.cast::<HtmlTextAreaElement>();

            if let Some(input) = input {
                input_value_handle.set(input.value());
                web_input.set(input.value());
            }
        })
    };

    let toplevel_class = use_style!(
        r#"
        font-size: 2rem;
        font-family: "Iosevka", monospace;
        "#
    );
    let title_class = use_style!("margin: 0;");
    let subtitle_class = use_style!("margin: 0;");
    let container_class = use_style!("display: flex; justify-content: flex-start;");
    let source_div_class = use_style!("padding-right: 1em;");
    let input_div_class = use_style!("");
    let input_class = use_style!("width: 50em; height: 55em;");

    html! {
        <div class={toplevel_class}>
            <h1 class={title_class}>{ "Brainlove" }</h1>
            <p class={subtitle_class}>{ "An interactive debugger for a " }<a href={"https://en.wikipedia.org/wiki/Brainfuck"}>{ "beautiful programming language" }</a></p>
            <div>
                <div class={container_class}>
                    <div class={source_div_class}>
                        <h2>{ "Source" }</h2>
                        <textarea class={input_class.clone()} ref={source_node_ref}
                            oninput={onsourceinput}
                            value={source_value.clone()}
                        />
                    </div>
                    <div class={input_div_class}>
                        <h2>{ "Input" }</h2>
                        <textarea class={input_class.clone()} ref={input_node_ref}
                            oninput={oninputinput}
                            value={input_value}
                        />
                    </div>
                </div>
                {interpreter_markup}
            </div>
        </div>
    }
}

fn main() {
    wasm_logger::init(wasm_logger::Config::default());

    yew::Renderer::<App>::new().render();
}
