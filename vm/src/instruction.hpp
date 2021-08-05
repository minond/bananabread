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
public:
  virtual string to_string() = 0;
  virtual ~Code() = default;
};

class Value : public Code {
public:
  enum Type {
    Invalid,
    Str,
    I32,
    Ref,
    Const,
  };

  static string type_to_string(Type type);

  Value(Type _type, string _label, string _value) :
    type(_type),
    label(_label),
    value(_value) {}

  Type get_type() { return type; }
  string get_label() { return label; }
  string get_value() { return value; }

  virtual string to_string() override {
    return string(".") +
      Value::type_to_string(type) + " " +
      label + ": " +
      value;
  }

private:
  Type type;
  string label;
  string value;
};

class Label : public Code {
public:
  Label(const string& _label) : label(_label) {}

  string get_label() { return label; }

  virtual string to_string() override { return label + ":"; }

private:
  string label;
};

class Instruction : public Code {
public:
  virtual ~Instruction() = default;
};

class Halt : public Instruction {
public:
  virtual string to_string() override { return "    halt"; }
};

class Swap : public Instruction {
public:
  virtual string to_string() override { return "    swap"; }
};

class Ret : public Instruction {
public:
  virtual string to_string() override { return "    ret"; }
};

class Println : public Instruction {
public:
  virtual string to_string() override { return "    println"; }
};

class Push : public Instruction {
public:
  Push(Value::Type _type, const string& _value) :
    type(_type),
    value(_value) {}

  Value::Type get_type() { return type; }
  string get_value() { return value; }

  virtual string to_string() override {
    return "    push " +
      Value::type_to_string(type) + ", " +
      value;
  }

private:
  Value::Type type;
  string value;
};

class Call : public Instruction {
public:
  Call(const string& _label) : label(_label) {}

  string get_label() { return label; }

  virtual string to_string() override { return "    call " + label; }

private:
  string label;
};

class Frame : public Instruction {
public:
  Frame(int _argc) : argc(_argc) {}

  int get_argc() { return argc; }

  virtual string to_string() override { return "    frame " + std::to_string(argc); }

private:
  int argc;
};

class Store : public Instruction {
public:
  Store(Value::Type _type, const string& _label) :
    type(_type),
    label(_label) {}

  Value::Type get_type() { return type; }
  string get_label() { return label; }

  virtual string to_string() override {
    return "    store " +
      Value::type_to_string(type) + ", " +
      label;
  }

private:
  Value::Type type;
  string label;
};

class Load : public Instruction {
public:
  Load(Value::Type _type, const string& _label) :
    type(_type),
    label(_label) {}

  Value::Type get_type() { return type; }
  string get_label() { return label; }

  virtual string to_string() override {
    return "    load " +
      Value::type_to_string(type) + ", " +
      label;
  }

private:
  Value::Type type;
  string label;
};

} // namespace Instruction
} // namespace Bananabread
