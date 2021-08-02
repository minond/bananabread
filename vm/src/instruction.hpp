namespace Bananabread {
namespace Instruction {

struct Label {
  std::string data;
};

enum class ValueType {
  Str,
  Ref,
  Const,
};

struct Value {
  ValueType type;
  std::string data;
};

enum class OpcodeType {
  Halt,
  Push,
  Call,
  Frame,
  Swap,
  Store,
  Load,
  Println,
  Ret,
};

struct PushInstruction {
  ValueType type;
  std::string value;
};

struct CallInstruction {
  std::string label;
};

struct FrameInstruction {
  int argc;
};

struct StoreInstruction {
  ValueType type;
  std::string value;
};

struct LoadInstruction {
  ValueType type;
  std::string value;
};

struct Instruction {
  OpcodeType type;
  union {
    PushInstruction push;
    CallInstruction call;
    FrameInstruction frame;
    StoreInstruction store;
    LoadInstruction load;
  };
};

enum class CodeType {
  Label,
  Value,
  Instruction,
};

struct Code {
  CodeType type;

  union {
    Label label;
    Value value;
    Instruction instruction;
  };
};

} // namespace Instruction
} // namespace Bananabread
