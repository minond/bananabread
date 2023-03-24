#include "instruction.hpp"

#include <iostream>

namespace Bananabread {
namespace Instruction {

using std::string;

string Value::type_to_string(Value::Type type) {
  switch (type) {
    case Value::Type::Invalid:
      return "Invalid";
    case Value::Type::Str:
      return "Str";
    case Value::Type::I32:
      return "I32";
    case Value::Type::Ref:
      return "Ref";
    case Value::Type::Const:
      return "Const";
  }
}

} // namespace Instruction
} // namespace Bananabread
