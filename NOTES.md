- Interpeter lib
  - Start out by creating an interpreter that accepts a file or reads from stdin.
    - Strip irrelevant characters from the input and represent actual code as enum; preprocess jump indices. Maybe include source information in a separate vector to enable printing later on.
    - Slightly inefficient to include jump indices inline; oh well.
  - Flesh out interpreter interface.
    - Run, step and breakpoint?
    - Public interpreter state.
    - Inject a put_char?
- Driver "binaries"
  - Get a command-line repl working
    - rustyline for reading inputs
    - Commands
      - help: print help text
      - step: step
      - run: run until breakpoint/completion
      - break: add a breakpoint at the current "instruction"
      - exit: exit
  - Yew!
    - We'll do it live :)

Discussion:

- If we change the code in the input field and hit continue, could we maintain
  the machine state and continue running with a modified program?
- Potentially want to include time-travel style functionality?
