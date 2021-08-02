#include <iostream>
#include <vector>
#include <sstream>

#include "instruction.hpp"
#include "parsing.hpp"


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
  auto lines = Bananabread::Parsing::split_lines(program);

  for (auto line : lines)
    Bananabread::Parsing::parse_line(line);

  return 0;
}
