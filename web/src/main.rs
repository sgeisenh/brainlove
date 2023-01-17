use anyhow::{anyhow, Result};
use interpreter::Interpreter;
use std::{
    cell::RefCell,
    fmt::Display,
    io::{Read, Write},
    rc::Rc,
};
use web_sys::HtmlTextAreaElement;
use yew::prelude::*;

pub enum InterpMsg {
    Step,
    Break(usize),
    Unbreak(usize),
    Run,
}

#[derive(Clone)]
pub struct WebInput {
    input: Rc<RefCell<Vec<u8>>>,
    pos: usize,
}

impl WebInput {
    fn new(input: Rc<RefCell<Vec<u8>>>) -> Self {
        Self { input, pos: 0 }
    }
}

impl Read for WebInput {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let inner = self.input.borrow();
        if self.pos >= inner.len() {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                anyhow!("position out of bounds"),
            ));
        }
        (&self.input.borrow()[self.pos..])
            .read(buf)
            .map(|num_bytes| {
                self.pos += num_bytes;
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
            InterpMsg::Break(offset) => {
                self.0 = interpreter.add_breakpoint(Some(offset));
                true
            }
            InterpMsg::Unbreak(offset) => {
                interpreter.remove_breakpoint(Some(offset));
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
        let string_to_display = interpreter.borrow().to_string();
        let set_bp = {
            let interpreter = interpreter.clone();
            move |_| {
                let bp = interpreter.borrow().get_ip();
                InterpMsg::Break(bp)
            }
        };
        let unset_bp = {
            let interpreter = interpreter.clone();
            move |_| {
                let bp = interpreter.borrow().get_ip();
                InterpMsg::Unbreak(bp)
            }
        };
        html! {
            <>
                <code>
                    {string_to_display}
                </code>
                <div>
                    <button onclick={ctx.link().callback(|_| InterpMsg::Step)}>
                        { "step" }
                    </button>
                    <button onclick={ctx.link().callback(set_bp)}>
                        { "break" }
                    </button>
                    <button onclick={ctx.link().callback(unset_bp)}>
                        { "unbreak" }
                    </button>
                    <button onclick={ctx.link().callback(|_| InterpMsg::Run)}>
                        { "run" }
                    </button>
                </div>
            </>
        }
    }
}

const HELLO_WORLD: &'static str = r#"
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

#[function_component]
fn App() -> Html {
    let source_node_ref = use_node_ref();

    let input = Rc::new(RefCell::new(b"wat".to_vec()));
    let source_value_handle = use_state(|| HELLO_WORLD.to_string());
    let source_value = (*source_value_handle).clone();
    let interpreter_handle = use_state({
        let input = input.clone();
        || -> Result<WebInterpreter> {
            Ok(Rc::new(RefCell::new(Interpreter::new(
                &source_value.bytes().collect::<Vec<_>>(),
                WebInput::new(input),
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

    let oninput = {
        let source_node_ref = source_node_ref.clone();
        let input = input.clone();

        Callback::from(move |_| {
            let source = source_node_ref.cast::<HtmlTextAreaElement>();

            if let Some(source) = source {
                source_value_handle.set(source.value());
                interpreter_handle.set(
                    Interpreter::new(
                        &source.value().into_bytes(),
                        WebInput::new(input.clone()),
                        WebOutput::default(),
                    )
                    .map(|interpreter| Rc::new(RefCell::new(interpreter))),
                );
            }
        })
    };

    html! {
        <>
            <textarea ref={source_node_ref}
                {oninput}
                id="my-input"
                value={source_value.clone()}
            />
            <br/>
            {interpreter_markup}
        </>
    }
}

fn main() {
    yew::Renderer::<App>::new().render();
}
