#include "vm.hpp"
#include "dispatch.hpp"
#include "value.hpp"

#include <stack>

namespace Bananabread {
namespace Handlers {

using std::stack;

Dispatch::Action* handle(Instruction::Code* code, VM::Registers reg, stack<Value::Base> stack);

} // namespace Handlers
} // namespace Bananabread
