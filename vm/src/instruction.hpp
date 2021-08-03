#pragma once

#include <iostream>

namespace Bananabread {
namespace Instruction {

using std::string;

enum class Type {
  Label,
  Value,
  Instruction,
};

class Code {
};

class Value : public Code {
public:
  enum class Type {
    Str,
    Ref,
    Const,
  };

  Value(Type _type, string _label, string _value) :
    type(_type),
    label(_label),
    value(_value) {}

  Type get_type() { return type; }
  string get_label() { return label; }
  string get_value() { return value; }

private:
  Type type;
  string label;
  string value;
};

class Label : public Code {
public:
  Label(const string& _label) : label(_label) {}

  string get_label() { return label; }

private:
  string label;
};

class Instruction : public Code {
};

class Halt : public Instruction {
};

class Push : public Instruction {
public:
  Push(Value::Type _type, const string& _value) :
    type(_type),
    value(_value) {}

  Value::Type get_type() { return type; }
  string get_value() { return value; }

private:
  Value::Type type;
  string value;
};

class Call : public Instruction {
public:
  Call(const string& _label) : label(_label) {}

  string get_label() { return label; }

private:
  string label;
};

class Frame : public Instruction {
public:
  Frame(int _argc) : argc(_argc) {}

  int get_argc() { return argc; }

private:
  int argc;
};

class Store : public Instruction {
public:
  Store(Value::Type _type, const string& _value) :
    type(_type),
    value(_value) {}

  Value::Type get_type() { return type; }
  string get_value() { return value; }

private:
  Value::Type type;
  string value;
};

class Load : public Instruction {
public:
  Load(Value::Type _type, const string& _value) :
    type(_type),
    value(_value) {}

  Value::Type get_type() { return type; }
  string get_value() { return value; }

private:
  Value::Type type;
  string value;
};

} // namespace Instruction
} // namespace Bananabread
