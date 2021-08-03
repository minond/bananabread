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

Instruction::Type deduce_instruction_type(const string& token) {
  if (token.ends_with(":")) {
    return Instruction::Type::Label;
  } else if (token.starts_with(".")) {
    return Instruction::Type::Value;
  } else {
    return Instruction::Type::Instruction;
  }
}

optional<Instruction::Value::Type> deduce_value_type(const string& token) {
  if (token == ".Str") {
    return optional{Instruction::Value::Type::Str};
  } else if (token == ".Ref") {
    return optional{Instruction::Value::Type::Ref};
  } else if (token == ".Const") {
    return optional{Instruction::Value::Type::Const};
  } else {
    return std::nullopt;
  }
}

optional<Instruction::Label> parse_label(vector<string> tokens) {
  if (tokens.size() != 1) {
    return std::nullopt;
  }

  auto token = tokens.at(0);
  auto label = clean_label(token);

  return optional{Instruction::Label(label)};
}

optional<Instruction::Value> parse_value(vector<string> tokens) {
  if (tokens.size() < 3) {
    return std::nullopt;
  }

  auto type = deduce_value_type(tokens.at(0));
  if (!type.has_value()) {
    return std::nullopt;
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

  return optional{Instruction::Value(type.value(), label, value.str())};
}

// TODO
optional<Instruction::Instruction> parse_instruction(vector<string> tokens) {
  return optional{Instruction::Halt()};
}

optional<Instruction::Code> parse_line(const string& line) {
    auto tokens = split_tokens(line);
    if (tokens.size() < 1) {
      return std::nullopt;
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

vector<Instruction::Code> parse(const string& program) {
  auto lines = split_lines(program);
  auto codes = vector<Instruction::Code>();

  for (auto line : lines) {
    auto code = parse_line(line);
    if (code.has_value()) {
      codes.push_back(code.value());
    }
  }

  return codes;
}

} // namespace Parsing
} // namespace Bananabread
