#pragma once

#include <string>

namespace Bananabread {
namespace Dispatch {

using std::string;

class Action {
};

class Cont : public Action {
};

class Stop : public Action {
};

class Goto : public Action {
public:
  Goto(string _label) : label(_label) {}

  string get_label() { return label; }

private:
  string label;
};

class Jump : public Action {
public:
  Jump(int _index) : index(_index) {}

  int get_index() { return index; }

private:
  int index;
};

class Error : public Action {
public:
  Error(string _message) : message(_message) {}

  string get_message() { return message; }

private:
  string message;
};

} // namespace Dispatch
} // namespace Bananabread
