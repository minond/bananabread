#include <iostream>
#include <vector>
#include <sstream>

#include "instruction.hpp"


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

std::vector<std::string> split_lines(const std::string& str) {
  auto lines = std::vector<std::string>();
  auto ss = std::stringstream(str);

  for (std::string line; std::getline(ss, line, '\n');) {
    lines.push_back(line);
  }

  return lines;
}

std::vector<std::string> split_words(const std::string& str) {
  auto words = std::vector<std::string>();

  return words;
}

void parse_line(const std::string& line) {
    std::cout << "parsing " << line << std::endl;

    auto words = split_words(line);

    for (auto word : words) {
      std::cout << word << std::endl;
    }
}

int main() {
  auto lines = split_lines(program);

  for (auto line : lines)
    parse_line(line);

  return 0;
}
