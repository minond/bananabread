#include <iostream>

#include "instruction.hpp"
#include "parsing.hpp"
#include "vm.hpp"


std::string program =
  "main:\n"
  "    push      I32, 10\n"
  "    load      I32, main.println\n"
  "    call      main.loop\n"
  "    halt\n"
  "main.-:\n"
  "    frame     2\n"
  "    swap\n"
  "    store     I32, main.-.b\n"
  "    swap\n"
  "    store     I32, main.-.a\n"
  "    load      I32, main.-.a\n"
  "    load      I32, main.-.b\n"
  "    sub       I32\n"
  "    swap\n"
  "    ret\n"
  "main.loop:\n"
  "    frame     2\n"
  "    swap\n"
  "    store     I32, main.loop.fn\n"
  "    swap\n"
  "    store     I32, main.loop.i\n"
  "    load      I32, main.loop.i\n"
  "    jz        else-B1tJ\n"
  "then-5wOC:\n"
  "    load      I32, main.loop.i\n"
  "    call      main.loop.fn\n"
  "    load      I32, main.loop.i\n"
  "    push      I32, 1\n"
  "    call      main.-\n"
  "    load      I32, main.loop.fn\n"
  "    call      main.loop\n"
  "    jmp       done-k06K\n"
  "else-B1tJ:\n"
  "    push      I32, 0\n"
  "    call      main.loop.fn\n"
  "done-k06K:\n"
  "    swap\n"
  "    ret\n"
  "main.println:\n"
  "    frame     1\n"
  "    swap\n"
  "    store     I32, main.println.x\n"
  "    load      Str, main.println.x\n"
  "    println\n"
  "    ret\n"
  "    swap\n"
  "    ret\n"
  "main.+:\n"
  "    frame     2\n"
  "    swap\n"
  "    store     I32, main.+.b\n"
  "    swap\n"
  "    store     I32, main.+.a\n"
  "    load      I32, main.+.a\n"
  "    load      I32, main.+.b\n"
  "    add       I32\n"
  "    swap\n"
  "    ret\n"
  ".Ref main.println:  main.println\n"
  ".Ref main.+:        main.+\n"
  ".Ref main.-:        main.-\n"
  ".Ref main.loop:     main.loop\n"
  "";


int main() {
  Bananabread::VM::Interpreter vm{
    Bananabread::Parsing::parse(program)
  };

  vm.run();

  return 0;
}
