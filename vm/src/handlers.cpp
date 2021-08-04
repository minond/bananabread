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

Dispatch::Action* handle_push(Instruction::Push* push, VM::Registers reg, stack<Value::Base*>* stack, map<string, Instruction::Value*> constants) {
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
      // TODO Handle invalid/missing constants.
      auto value = Value::from_instruction(constants[push->get_value()]);
      stack->push(value);
      return new Dispatch::Cont();
  }
}

// TODO: Add frame
Dispatch::Action* handle_call(Instruction::Call* call, VM::Registers reg, stack<Value::Base*>* stack, map<string, Instruction::Value*> constants) {
  stack->push(new Value::I32{reg.pc()});
  return new Dispatch::Goto(call->get_label());
}

// TODO: Add frame
Dispatch::Action* handle_swap(Instruction::Swap* swap, VM::Registers reg, stack<Value::Base*>* stack, map<string, Instruction::Value*> constants) {
  auto a = stack->top();
  stack->pop();
  auto b = stack->top();
  stack->pop();

  stack->push(a);
  stack->push(b);

  return new Dispatch::Cont();
}

Dispatch::Action* handle(Instruction::Code* code, VM::Registers reg, stack<Value::Base*>* stack, map<string, Instruction::Value*> constants) {
  if (dynamic_cast<Instruction::Label*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Value*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Halt*>(code)) {
    return new Dispatch::Stop();
  } else if (auto push = dynamic_cast<Instruction::Push*>(code)) {
    return handle_push(push, reg, stack, constants);
  } else if (auto call = dynamic_cast<Instruction::Call*>(code)) {
    return handle_call(call, reg, stack, constants);
  } else if (dynamic_cast<Instruction::Frame*>(code)) {
    return new Dispatch::Cont();
  } else if (auto swap = dynamic_cast<Instruction::Swap*>(code)) {
    return handle_swap(swap, reg, stack, constants);
  }

  return new Dispatch::Error("internal error: unhandled instruction");
}

} // namespace Handlers
} // namespace Bananabread
