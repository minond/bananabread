#include "instruction.hpp"

#include <map>
#include <vector>
#include <string>

namespace Bananabread {
namespace VM {

using std::map;
using std::vector;
using std::string;

typedef vector< Instruction::Code* > Codes;

class Interpreter {
public:
  Interpreter(Codes _codes) :
    codes(_codes) {}

  void run();

private:
  Codes codes;

  map<string, int> get_labels();
  map<string, Instruction::Value*> get_constants();
};

} // namespace VM
} // namespace Bananabread
