#include <iostream>
#include <vector>
#include <sstream>

namespace Bananabread {
namespace Parsing {

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

} // namespace Parsing
} // namespace Bananabread
