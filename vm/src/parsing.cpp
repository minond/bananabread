#include "parsing.hpp"

#include <iostream>
#include <vector>
#include <sstream>

namespace Bananabread {
namespace Parsing {

using std::string;
using std::vector;
using std::stringstream;

vector<string> split_lines(const string& str) {
  auto lines = vector<string>();
  auto ss = stringstream(str);

  for (string line; std::getline(ss, line, '\n');) {
    lines.push_back(line);
  }

  return lines;
}

vector<string> split_words(const string& str) {
  auto ss = stringstream(str);
  auto words = vector<string>{
    std::istream_iterator<string>{ss},
    std::istream_iterator<string>{},
  };

  return words;
}

void parse_line(const string& line) {
    std::cout << "parsing " << line << std::endl;

    auto words = split_words(line);

    for (auto word : words) {
      std::cout << "word: " << word << std::endl;
    }
}

void parse(const string& program) {
  auto lines = split_lines(program);

  for (auto line : lines) {
    parse_line(line);
  }
}

} // namespace Parsing
} // namespace Bananabread
