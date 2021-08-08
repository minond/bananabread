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

  auto state = Handlers::State{
    .reg = &reg,
    .frames = &frames,
    .stack = &stack,
    .constants = constants,
  };

  while (reg.pc() != -1) {
    auto code = codes.at(reg.pc());
    auto next = Handlers::handle(code, &state);

#if DEBUG
    std::cout << "running " << code->to_string() << std::endl;
#endif

    if (dynamic_cast<Dispatch::Stop*>(next)) {
      reg.set_pc(-1);
    } else if (auto error = dynamic_cast<Dispatch::Error*>(next)) {
      std::cout << "error: " << error->get_message() << std::endl;
      reg.set_pc(-1);
    } else if (dynamic_cast<Dispatch::Cont*>(next)) {
      reg.inc_pc();
    } else if (auto gt = dynamic_cast<Dispatch::Goto*>(next)) {
      auto label = gt->get_label();
      if (labels.contains(label)) {
        reg.set_pc(labels.at(label));
      } else {
        std::cout << "error: bad goto, missing label: " << gt->get_label() << std::endl;
        reg.set_pc(-1);
      }
    } else if (auto jm = dynamic_cast<Dispatch::Jump*>(next)) {
      reg.set_pc(jm->get_index());
    } else {
      std::cout << "error: bad dispatch" << std::endl;
      reg.set_pc(-1);
    }
  }
}

} // namespace VM
} // namespace Bananabread
