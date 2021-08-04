#include "value.hpp"
#include "instruction.hpp"

namespace Bananabread {
namespace Value {

Base* from_instruction(Instruction::Value* inst) {
  switch (inst->get_type()) {
    case Instruction::Value::Type::Invalid:
      return nullptr;
    case Instruction::Value::Type::Str:
      return new Str{inst->get_value()};
    case Instruction::Value::Type::I32:
      try {
        auto value = std::stoi(inst->get_value());
        return new I32{value};
      } catch (std::invalid_argument) {
        return nullptr;
      }
    case Instruction::Value::Type::Ref:
      return nullptr;
    case Instruction::Value::Type::Const:
      return nullptr;
  }
}

} // namespace Value
} // namespace Bananabread
