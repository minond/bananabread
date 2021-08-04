#include "vm.hpp"
#include "dispatch.hpp"
#include "handlers.hpp"

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

  while (reg.pc() != -1) {
    auto code = codes.at(reg.pc());
    auto action = Handlers::handle(code, reg);

    std::cout << "running " << code->to_string() << std::endl;

    if (static_cast<Dispatch::Stop*>(action)) {
      reg.set_pc(-1);
    } else if (static_cast<Dispatch::Error*>(action)) {
      reg.set_pc(-1);
    } else if (static_cast<Dispatch::Cont*>(action)) {
      reg.inc_pc();
    }
  }
}

} // namespace VM
} // namespace Bananabread
