#pragma once

#include "instruction.hpp"

#include <string>

namespace Bananabread {
namespace Value {

using std::string;

class Base {
public:
  virtual ~Base() = default;
  virtual string to_string() = 0;
};

class I32 : public Base {
public:
  I32(int _value) : value(_value) {}

  int get_int() { return value; }

  virtual string to_string() override { return std::to_string(value); }

private:
  int value;
};

class Str : public Base {
public:
  Str(string _value) : value(_value) {}

  string get_string() { return value; }

  virtual string to_string() override { return value; }

private:
  string value;
};

Base* from_instruction(Instruction::Value* inst);

} // namespace Value
} // namespace Bananabread
