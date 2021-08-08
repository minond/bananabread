#include "parsing.hpp"
#include "instruction.hpp"

#include <iostream>
#include <vector>
#include <sstream>
#include <optional>

namespace Bananabread {
namespace Parsing {

using std::string;
using std::vector;
using std::stringstream;
using std::ostringstream;
using std::optional;

enum class LIKind {
  Jz,
  Jmp,
};

enum class TIKind {
  Add,
  Sub,
};

enum class TVIKind {
  Push,
  Store,
  Load,
};

vector<string> split_lines(const string& str) {
  auto lines = vector<string>();
  auto ss = stringstream(str);

  for (string line; std::getline(ss, line, '\n');) {
    lines.push_back(line);
  }

  return lines;
}

vector<string> split_tokens(const string& str) {
  auto ss = stringstream(str);
  auto tokens = vector<string>(
    std::istream_iterator<string>(ss),
    std::istream_iterator<string>()
  );

  return tokens;
}

string clean_label(string token) {
  return token.erase(token.size() - 1);
}

string clean_type(string token) {
  if (token.ends_with(",")) {
    token = token.erase(token.size() - 1);
  }

  if (token.starts_with(".")) {
    token = token.erase(0, 1);
  }

  return token;
}

Instruction::Type deduce_instruction_type(const string& token) {
  if (token.ends_with(":")) {
    return Instruction::Type::Label;
  } else if (token.starts_with(".")) {
    return Instruction::Type::Value;
  } else {
    return Instruction::Type::Instruction;
  }
}

Instruction::Value::Type deduce_value_type(const string& token) {
  if (token == "Str") {
    return Instruction::Value::Type::Str;
  } else if (token == "I32") {
    return Instruction::Value::Type::I32;
  } else if (token == "Ref") {
    return Instruction::Value::Type::Ref;
  } else if (token == "Const") {
    return Instruction::Value::Type::Const;
  } else {
    return Instruction::Value::Type::Invalid;
  }
}

Instruction::Label* parse_label(vector<string> tokens) {
  if (tokens.size() != 1) {
    return nullptr;
  }

  auto token = tokens.at(0);
  auto label = clean_label(token);

  return new Instruction::Label(label);
}

Instruction::Value* parse_value(vector<string> tokens) {
  if (tokens.size() < 3) {
    return nullptr;
  }

  auto type = deduce_value_type(clean_type(tokens.at(0)));
  if (type == Instruction::Value::Type::Invalid) {
    return nullptr;
  }

  auto label = clean_label(tokens.at(1));
  auto value_tokens = vector<string>(tokens.size() - 2);
  std::copy(tokens.begin() + 2,
            tokens.end(),
            value_tokens.begin());
  ostringstream value;
  std::copy(value_tokens.begin(),
            value_tokens.end(),
            std::ostream_iterator<string>(value, " "));

  auto str = value.str();
  str = str.erase(str.size() - 1);
  return new Instruction::Value(type, label, str);
}

Instruction::Frame* parse_frame_instruction(vector<string> tokens) {
  if (tokens.size() != 2) {
    return nullptr;
  }

  try {
    auto argc = std::stoi(tokens.at(1));
    return new Instruction::Frame(argc);
  } catch (std::invalid_argument) {
    return nullptr;
  }
}

Instruction::Call* parse_call_instruction(vector<string> tokens) {
  if (tokens.size() != 2) {
    return nullptr;
  }

  return new Instruction::Call(tokens.at(1));
}

Instruction::Instruction* parse_labeled_instruction(LIKind kind, vector<string> tokens) {
  if (tokens.size() != 2) {
    return nullptr;
  }

  auto label = tokens.at(1);
  switch (kind) {
    case LIKind::Jz:
      return new Instruction::Jz(label);
    case LIKind::Jmp:
      return new Instruction::Jmp(label);
  }
}

Instruction::Instruction* parse_typed_instruction(TIKind kind, vector<string> tokens) {
  if (tokens.size() != 2) {
    return nullptr;
  }

  auto type = deduce_value_type(clean_type(tokens.at(1)));
  if (type == Instruction::Value::Type::Invalid) {
    return nullptr;
  }

  switch (kind) {
    case TIKind::Add:
      return new Instruction::Add(type);
    case TIKind::Sub:
      return new Instruction::Sub(type);
  }
}

Instruction::Instruction* parse_typed_value_instruction(TVIKind kind, vector<string> tokens) {
  if (tokens.size() != 3) {
    return nullptr;
  }

  auto type = deduce_value_type(clean_type(tokens.at(1)));
  if (type == Instruction::Value::Type::Invalid) {
    return nullptr;
  }

  switch (kind) {
    case TVIKind::Push:
      return new Instruction::Push(type, tokens.at(2));
    case TVIKind::Store:
      return new Instruction::Store(type, tokens.at(2));
    case TVIKind::Load:
      return new Instruction::Load(type, tokens.at(2));
  }
}

Instruction::Instruction* parse_instruction(vector<string> tokens) {
  if (tokens.size() < 1) {
    return nullptr;
  }

  auto head = tokens.at(0);

  if (head == "halt") {
    return new Instruction::Halt();
  } else if (head == "swap") {
    return new Instruction::Swap();
  } else if (head == "ret") {
    return new Instruction::Ret();
  } else if (head == "println") {
    return new Instruction::Println();
  } else if (head == "frame") {
    return parse_frame_instruction(tokens);
  } else if (head == "call") {
    return parse_call_instruction(tokens);
  } else if (head == "add") {
    return parse_typed_instruction(TIKind::Add, tokens);
  } else if (head == "sub") {
    return parse_typed_instruction(TIKind::Sub, tokens);
  } else if (head == "push") {
    return parse_typed_value_instruction(TVIKind::Push, tokens);
  } else if (head == "store") {
    return parse_typed_value_instruction(TVIKind::Store, tokens);
  } else if (head == "load") {
    return parse_typed_value_instruction(TVIKind::Load, tokens);
  } else if (head == "jz") {
    return parse_labeled_instruction(LIKind::Jz, tokens);
  } else if (head == "jmp") {
    return parse_labeled_instruction(LIKind::Jmp, tokens);
  }

  return nullptr;
}

Instruction::Code* parse_line(const string& line) {
    auto tokens = split_tokens(line);
    if (tokens.size() < 1) {
      return nullptr;
    }

    auto type = deduce_instruction_type(tokens.at(0));
    switch (type) {
      case Instruction::Type::Label:
        return parse_label(tokens);
      case Instruction::Type::Value:
        return parse_value(tokens);
      case Instruction::Type::Instruction:
        return parse_instruction(tokens);
    }
}

vector<Instruction::Code*> parse(const string& program) {
  auto lines = split_lines(program);
  auto codes = vector<Instruction::Code*>();

  for (auto line : lines) {
    auto code = parse_line(line);
    if (code) {
      codes.push_back(code);
    }
  }

  return codes;
}

} // namespace Parsing
} // namespace Bananabread
