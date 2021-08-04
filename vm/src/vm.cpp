#include "vm.hpp"

#include <map>
#include <vector>
#include <string>

namespace Bananabread {
namespace VM {

using std::map;
using std::vector;
using std::string;

map<string, int> Interpreter::get_labels() {
  map<string, int> labels{};

  auto i = 0;
  auto len = codes.size();
  for (; i < len; i++) {
    if (auto label = dynamic_cast<Instruction::Label*>(codes.at(i))) {
      labels[label->get_label()] = i;
    }
  }

  return labels;
}

map<string, Instruction::Value*> Interpreter::get_constants() {
  map<string, Instruction::Value*> constants{};

  auto i = 0;
  auto len = codes.size();
  for (; i < len; i++) {
    if (auto constant = dynamic_cast<Instruction::Value*>(codes.at(i))) {
      constants[constant->get_label()] = constant;
    }
  }

  return constants;
}

void Interpreter::run() {
  auto labels = get_labels();
  auto constants = get_constants();

  for (auto code : codes) {
    std::cout << code->to_string() << std::endl;
  }
}

} // namespace VM
} // namespace Bananabread
