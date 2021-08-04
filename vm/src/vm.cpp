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
    std::cout << "running " << code->to_string() << std::endl;
    auto action = Handlers::handle(code, reg, &stack, constants);

    if (dynamic_cast<Dispatch::Stop*>(action)) {
      reg.set_pc(-1);
    } else if (auto error = dynamic_cast<Dispatch::Error*>(action)) {
      std::cout << "error: " << error->get_message() << std::endl;
      reg.set_pc(-1);
    } else if (dynamic_cast<Dispatch::Cont*>(action)) {
      reg.inc_pc();
    } else if (auto gt = dynamic_cast<Dispatch::Goto*>(action)) {
      auto label_index = labels[gt->get_label()];
      reg.set_pc(label_index);
    }
  }
}

} // namespace VM
} // namespace Bananabread
