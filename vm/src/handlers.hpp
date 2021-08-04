#include "vm.hpp"
#include "dispatch.hpp"

namespace Bananabread {
namespace Handlers {

Dispatch::Action* handle(Instruction::Code* code, VM::Registers reg);

} // namespace Handlers
} // namespace Bananabread
