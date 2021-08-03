#pragma once

#include "instruction.hpp"

#include <iostream>
#include <vector>

namespace Bananabread {
namespace Parsing {

enum class TVIKind {
  Push,
  Store,
  Load,
};

std::vector<Instruction::Code*> parse(const std::string& program);

} // namespace Parsing
} // namespace Bananabread
