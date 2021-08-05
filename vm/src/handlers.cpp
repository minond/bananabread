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

Dispatch::Action* handle_push(Instruction::Push* push, State* state) {
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
      auto value = Value::from_instruction(state->constants[push->get_value()]);
      state->stack->push(value);
      return new Dispatch::Cont();
  }
}

Dispatch::Action* handle_call(Instruction::Call* call, State* state) {
  state->stack->push(new Value::I32{state->reg->pc()});
  // TODO: Add frame
  return new Dispatch::Goto(call->get_label());
}

// TODO: Add frame
Dispatch::Action* handle_swap(Instruction::Swap* swap, State* state) {
  auto a = state->stack->top();
  state->stack->pop();
  auto b = state->stack->top();
  state->stack->pop();

  state->stack->push(a);
  state->stack->push(b);

  return new Dispatch::Cont();
}

Dispatch::Action* handle(Instruction::Code* code, State* state) {
  if (dynamic_cast<Instruction::Label*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Value*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Halt*>(code)) {
    return new Dispatch::Stop();
  } else if (auto push = dynamic_cast<Instruction::Push*>(code)) {
    return handle_push(push, state);
  } else if (auto call = dynamic_cast<Instruction::Call*>(code)) {
    return handle_call(call, state);
  } else if (dynamic_cast<Instruction::Frame*>(code)) {
    return new Dispatch::Cont();
  } else if (auto swap = dynamic_cast<Instruction::Swap*>(code)) {
    return handle_swap(swap, state);
  }

  return new Dispatch::Error("internal error: unhandled instruction");
}

} // namespace Handlers
} // namespace Bananabread
