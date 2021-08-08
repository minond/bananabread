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
      try {
        auto inner = std::stoi(push->get_value());
        auto value = new Value::I32(inner);
        state->stack->push(value);
        return new Dispatch::Cont();
      } catch (std::invalid_argument) {
        return new Dispatch::Error("bad push: invalid i32 value");
      }
    case Instruction::Value::Type::Ref:
      return new Dispatch::Error("unimplemented: push ref");
    case Instruction::Value::Type::Const:
      // TODO Convert constants to runtime values before evaluating expression
      // and handle invalid/missing constants.
      auto value = Value::from_instruction(state->constants[push->get_value()]);
      state->stack->push(value);
      return new Dispatch::Cont();
  }
}

// TODO handle Some(Id), Some(Scope), and Some(Invalid).
Dispatch::Action* handle_call(Instruction::Call* call, State* state) {
  auto ref = state->frames->get_curr()->get(call->get_label());

  if (!ref) {
    state->stack->push(new Value::I32{state->reg->pc() + 1});
    state->frames->move_to_next();
    return new Dispatch::Goto(call->get_label());
  }

  return new Dispatch::Error("bad call");
}

Dispatch::Action* handle_swap(Instruction::Swap* swap, State* state) {
  auto a = state->stack->top();
  state->stack->pop();
  auto b = state->stack->top();
  state->stack->pop();

  state->stack->push(a);
  state->stack->push(b);

  return new Dispatch::Cont();
}

Dispatch::Action* handle_store(Instruction::Store* store, State* state) {
  state->frames->get_curr()->put(store->get_label(), state->stack->top());
  state->stack->pop();
  return new Dispatch::Cont();
}

Dispatch::Action* handle_load(Instruction::Load* load, State* state) {
  auto ref = state->frames->get_curr()->get(load->get_label());

  if (ref) {
    state->stack->push(ref);
    return new Dispatch::Cont();
  }

  auto inst = state->constants[load->get_label()];
  if (!inst) {
    return new Dispatch::Error("missing const: " + load->get_label());
  }

  auto value = Value::from_instruction(inst);
  state->stack->push(value);
  return new Dispatch::Cont();
}

Dispatch::Action* handle_println(Instruction::Println* println, State* state) {
  auto value = state->stack->top();
  if (!value) {
    return new Dispatch::Error("missing stack value");
  }

  std::cout << value->to_string() << std::endl;
  state->stack->pop();
  return new Dispatch::Cont();
}

Dispatch::Action* handle_ret(Instruction::Ret* ret, State* state) {
  auto value = state->stack->top();
  if (!value) {
    return new Dispatch::Error("bad ret: missing return address");
  }

  if (auto addr = dynamic_cast<Value::I32*>(value)) {
    state->stack->pop();
    state->frames->move_to_prev();
    return new Dispatch::Jump(addr->get_int());
  }

  return new Dispatch::Error("bad ret: invalid return address");
}

Dispatch::Action* handle_add(Instruction::Add* add, State* state) {
  auto rhs_value = state->stack->top();
  state->stack->pop();
  auto lhs_value = state->stack->top();
  state->stack->pop();

  if (auto rhs = dynamic_cast<Value::I32*>(rhs_value)) {
    if (auto lhs = dynamic_cast<Value::I32*>(lhs_value)) {
      auto result = lhs->get_int() + rhs->get_int();
      state->stack->push(new Value::I32(result));
      return new Dispatch::Cont();
    } else {
      return new Dispatch::Error("bad add: left hand side is not an int");
    }
  } else {
    return new Dispatch::Error("bad add: right hand side is not an int");
  }
}

Dispatch::Action* handle_sub(Instruction::Sub* sub, State* state) {
  auto rhs_value = state->stack->top();
  state->stack->pop();
  auto lhs_value = state->stack->top();
  state->stack->pop();

  if (auto rhs = dynamic_cast<Value::I32*>(rhs_value)) {
    if (auto lhs = dynamic_cast<Value::I32*>(lhs_value)) {
      auto result = lhs->get_int() - rhs->get_int();
      state->stack->push(new Value::I32(result));
      return new Dispatch::Cont();
    } else {
      return new Dispatch::Error("bad sub: left hand side is not an int");
    }
  } else {
    return new Dispatch::Error("bad sub: right hand side is not an int");
  }
}

Dispatch::Action* handle_jz(Instruction::Jz* jz, State* state) {
  auto value = state->stack->top();
  if (!value) {
    return new Dispatch::Error("bad jz: empty stack");
  }

  auto cond = dynamic_cast<Value::I32*>(value);
  if (!cond) {
    return new Dispatch::Error("bad jz: invalid head value");
  }

  if (cond->get_int() == 0) {
    return new Dispatch::Goto(jz->get_label());
  }

  return new Dispatch::Cont();
}

Dispatch::Action* handle_jmp(Instruction::Jmp* jmp, State* state) {
  return new Dispatch::Goto(jmp->get_label());
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
  } else if (auto store = dynamic_cast<Instruction::Store*>(code)) {
    return handle_store(store, state);
  } else if (auto load = dynamic_cast<Instruction::Load*>(code)) {
    return handle_load(load, state);
  } else if (auto println = dynamic_cast<Instruction::Println*>(code)) {
    return handle_println(println, state);
  } else if (auto ret = dynamic_cast<Instruction::Ret*>(code)) {
    return handle_ret(ret, state);
  } else if (auto add = dynamic_cast<Instruction::Add*>(code)) {
    return handle_add(add, state);
  } else if (auto sub = dynamic_cast<Instruction::Sub*>(code)) {
    return handle_sub(sub, state);
  } else if (auto jz = dynamic_cast<Instruction::Jz*>(code)) {
    return handle_jz(jz, state);
  } else if (auto jmp = dynamic_cast<Instruction::Jmp*>(code)) {
    return handle_jmp(jmp, state);
  }

  return new Dispatch::Error("internal error: unhandled instruction");
}

} // namespace Handlers
} // namespace Bananabread
