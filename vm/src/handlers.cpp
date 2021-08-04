#include "handlers.hpp"

namespace Bananabread {
namespace Handlers {

Dispatch::Action* handle(Instruction::Code* code, VM::Registers reg) {
  if (dynamic_cast<Instruction::Label*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Value*>(code)) {
    return new Dispatch::Cont();
  } else if (dynamic_cast<Instruction::Halt*>(code)) {
    return new Dispatch::Stop();
  }

  return new Dispatch::Error("internal error: unhandled instruction");
}

} // namespace Handlers
} // namespace Bananabread
