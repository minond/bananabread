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

class Frame {
public:
  Frame(Frame* _parent) : parent(_parent) {
    env = new map<string, Value::Base*>{};
  }
  Frame(map<string, Value::Base*>* _env, Frame* _parent) :
    parent(_parent), env(_env) {}

  Frame* get_prev() { return parent; }
  Frame* get_next() { return new Frame(this); }

  Frame* from(Frame* other) { return new Frame(other->env, this); }

  void put(string label, Value::Base* value) {
    env->insert_or_assign(label, value);
  }

  Value::Base* get(string label) {
    if (env && env->contains(label)) {
      return env->at(label);
    } else if (parent) {
      return parent->get(label);
    }

    return nullptr;
  }

protected:
  Frame* parent;
  map<string, Value::Base*>* env;
};

class Frames {
public:
  Frames() : curr(new Frame(nullptr)) {}

  Frame* get_curr() { return curr; }

  void move_to_prev() { curr = curr->get_prev(); }
  void move_to_next() { curr = curr->get_next(); }

private:
  Frame* curr;
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
  Frames frames;
  stack<Value::Base*> stack;

  map<string, int> get_labels();
  map<string, Instruction::Value*> get_constants();
};

} // namespace VM
} // namespace Bananabread
