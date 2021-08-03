#include "instruction.hpp"

#include <iostream>
#include <vector>

namespace Bananabread {
namespace Parsing {

using std::vector;

vector<Instruction::Code> parse(const std::string& program);

} // namespace Parsing
} // namespace Bananabread
