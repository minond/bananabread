#pragma once

#include "instruction.hpp"
#include "value.hpp"

#include <map>
#include <vector>
#include <string>
#include <stack>

namespace Bananabread {
namespace VM {

using std::map;
using std::vector;
using std::string;
using std::stack;

typedef vector< Instruction::Code* > Codes;

class Registers {
public:
  int pc() { return _pc; }
  int lr() { return _lr; }
  int jm() { return _jm; }

  void inc_pc() { _pc += 1; }
  void inc_lr() { _lr += 1; }
  void inc_jm() { _jm += 1; }

  void set_pc(int value) { _pc = value; }
  void set_lr(int value) { _lr = value; }
  void set_jm(int value) { _jm = value; }

private:
  int _pc;
  int _lr;
  int _jm;
};

class Interpreter {
public:
  Interpreter(Codes _codes) : codes(_codes) {
    reg = Registers();
  }

  void run();

private:
  Codes codes;
  Registers reg;
  stack<Value::Base> stack;

  map<string, int> get_labels();
  map<string, Instruction::Value*> get_constants();
};

} // namespace VM
} // namespace Bananabread
