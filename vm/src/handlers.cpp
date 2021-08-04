#include "handlers.hpp"
#include "value.hpp"
#include "instruction.hpp"

#include <stack>
#include <string>
#include <map>

namespace Bananabread {
namespace Handlers {

using std::stack;
using std::string;
using std::map;

Dispatch::Action* handle_push(Instruction::Push* push, VM::Registers reg, stack<Value::Base*> stack, map<string, Instruction::Value*> constants) {
  switch (push->get_type()) {
    case Instruction::Value::Type::Invalid:
      return new Dispatch::Error("invalid type for push instruction");
    case Instruction::Value::Type::Str:
      return new Dispatch::Error("unimplemented: push str");
    case Instruction::Value::Type::I32:
      return new Dispatch::Error("unimplemented: push i32");
    case Instruction::Value::Type::Ref:
      return new Dispatch::Error("unimplemented: push ref");
    case Instruction::Value::Type::Const:
      // TODO Convert constants to runtime values before evaluating expression.
      auto value = Value::from_instruction(constants[push->get_value()]);
      stack.push(value);
      return new Dispatch::Cont();
  }
}

Dispatch::Action* handle(Instruction::Code* code, VM::Registers reg, stack<Value::Base*> stack, map<string, Instruction::Value*> constants) {
  if (dynamic_cast<Instruction::Label*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Value*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Halt*>(code)) {
    return new Dispatch::Stop();
  } else if (auto push = dynamic_cast<Instruction::Push*>(code)) {
    return handle_push(push, reg, stack, constants);
  }

  return new Dispatch::Error("internal error: unhandled instruction");
}

} // namespace Handlers
} // namespace Bananabread
