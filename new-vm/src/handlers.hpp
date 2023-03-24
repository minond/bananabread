#pragma once

#include "vm.hpp"
#include "dispatch.hpp"
#include "value.hpp"

#include <stack>
#include <string>
#include <map>

namespace Bananabread {
namespace Handlers {

using std::stack;
using std::string;
using std::map;

struct State {
  VM::Registers* reg;
  VM::Frames* frames;
  stack<Value::Base*>* stack;
  map<string, Instruction::Value*> constants;
};

Dispatch::Action* handle(Instruction::Code* code, State* state);

} // namespace Handlers
} // namespace Bananabread
