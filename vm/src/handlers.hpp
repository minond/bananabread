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

Dispatch::Action* handle(Instruction::Code* code, VM::Registers reg, stack<Value::Base*> stack, map<string, Instruction::Value*> constants);

} // namespace Handlers
} // namespace Bananabread
