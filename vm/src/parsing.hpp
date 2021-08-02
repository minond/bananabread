#include <iostream>

namespace Bananabread {
namespace Parsing {

void parse(const std::string& program);

void parse_line(const std::string& line);
std::vector<std::string> split_lines(const std::string& str);
std::vector<std::string> split_words(const std::string& str);

} // namespace Parsing
} // namespace Bananabread
