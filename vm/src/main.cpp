#include <iostream>

#include "instruction.hpp"
#include "parsing.hpp"
#include "vm.hpp"


std::string program =
  "main:\n"
  "    push      Const, str-2\n"
  "    call      main.println\n"
  "    halt\n"
  "main.println:\n"
  "    frame     1\n"
  "    swap\n"
  "    store     I32, main.println.x\n"
  "    load      Str, main.println.x\n"
  "    println\n"
  "    ret\n"
  "    swap\n"
  "    ret\n"
  ".Ref main.println:  main.println\n"
  ".Str str-2:         hello, world";


int main() {
  Bananabread::VM::Interpreter vm{
    Bananabread::Parsing::parse(program)
  };

  vm.run();

  return 0;
}
