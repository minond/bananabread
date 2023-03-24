# Bananabread

Bananabread is a programming language, compiler, and virtual machine
implementation. To get started, install
[sbt](https://www.scala-sbt.org/1.x/docs/Setup.html) and run `sbt` to load the
project. Once loaded, run any of the following tasks to execute the program:
`run`, `run debug`, `run print-opcodes`, `run print-state`.

The `run` task will compile and execute
[`compiler/lib/Sample.bb`](compiler/lib/Sample.bb) and since unfortunately I
haven't gotten around to implementing a repl yet, modifying this file is the
only way to interact with the language for now.

Bananabread is a straightforward language, but the feature that really excites
me about it is its ability to inline virtual machine instruction opcodes with
the code. This is what that looks like:

```
def +(a : I32, b : I32) : I32 =
  %[[
    load [I32] a
    load [I32] b
    add [I32]
  ]]
```

This is a function declaration for `+`, which has a signature of `I32, I32 ->
I32` as you'd expect, however it is implemented using Bananabread's VM
instructions. Code between `%[[` and `]]` are all instructions that the
compiler parses, checks, and then inlines into the emitted code that it
generates which is also the the VM.

Bananabread's VM is a stack machine but it does have some registers. It's
instruction set is simple, and takes a lot of inspiration from the JVM's own
instruction set. The opcodes are the following:

- `halt` halts the program.
- `jz <label>` pops the stack and jumps to `<label>` if the popped value is 0.
- `jmp <label>` jumps to `<label>`.
- `push <type> <value>` pushes a value on to the stack.
- `call <label>` stores certain registers on the stack and jumps to `<label>`.
- `call0` like `call` but jumps to the address stored in the `jm` register.
- `ret` restores the registers to their status before `call`/`call0` and jumps back to the caller.
- `frame <arg-count>` creates a call frame.
- `swap` swaps the two top items in the stack.
- `mov <register> [address] [offset] [address-register] [offset-register]` has a few forms:
    - `mov <register> <address> <offset>` look up `<address>` on the heap, add
      `<offset>` to it then store the value in `<register>`.
    - `mov <register> <address> <offset-register>` look up `<address>` on the
      heap, add the value found in `<offset-register>` to it then store the
      value in `<register>`.
    - `mov <register> <address-register>` read `<address-register>` and look up
      that address on the heap, then store the value in `<register>`.
    - `mov <register>` pop the stack and store the value in `<register>`.
- `stw <register>` pushes the value in `<register>` on the stack.
- `ldw register` pops a value from the stack and stores it in `<register>`.
- `load <type> <label>` pushes a constant on to the stack.
- `store <type> <label>` pops a value from the stack and stores it in the current frame.
- `println` pops a value from the stack and prints it to the screen.
- `concat` concatenates the top two items on the stack.
- `add <type>` adds the top two items on the stack.
- `sub <type>` subtracts the top two items on the stack.


A working version of the compiler and VM can be found in the `compiler`
directory. There is an imcomplete version of the VM in the `new-vm` directory
as well, but it doesn't do much right now. This is a sample program and the
resulting instructions:

```
module Sample

import Prelude exposing (+, println)

operator(infix, 2, '+)

def a = 22
def b = 20

println(a + b)
```

```
main:
        push      I32, 22
        store     I32, main.a
        push      I32, 20
        store     I32, main.b
        load      I32, main.a
        load      I32, main.b
        call      main.+
        call      main.println
        halt
main.println:
        frame     1
        swap
        store     Ref, main.println.x
        stw       Ebp
        stw       Esp
        ldw       Ebp
        load      Str, main.println.x
        println
        load      Str, main.println.x
        ldw       Rt
        stw       Ebp
        ldw       Esp
        ldw       Ebp
        stw       Rt
        swap
        ret
main.+:
        frame     2
        swap
        store     I32, main.+.b
        swap
        store     I32, main.+.a
        stw       Ebp
        stw       Esp
        ldw       Ebp
        load      I32, main.+.a
        load      I32, main.+.b
        add       I32
        ldw       Rt
        stw       Ebp
        ldw       Esp
        ldw       Ebp
        stw       Rt
        swap
        ret
```
