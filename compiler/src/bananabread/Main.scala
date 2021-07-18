package bananabread

import parsing.language.{Syntax, tokenize, parse}
import ir.typeless
import runtime.Interpreter
import runtime.instruction.pp


def main(args: Array[String]) =
  val syntax = Syntax.withPrefix(1, "-")
                     .withPrefix(1, "*")
                     .withPrefix(1, "∀")
                     .withPrefix(1, "!")
                     .withPrefix(1, "opcode")
                     .withInfix(4, "^")
                     .withInfix(3, "*")
                     .withInfix(3, "/")
                     .withInfix(3, "%")
                     .withInfix(2, "+")
                     .withInfix(2, "-")
                     .withInfix(0, "|>")
                     .withInfix(0, "_o_")
                     .withInfix(10, "∈")
                     .withInfix(99, "^")
                     .withInfix(2, ":")
                     .withInfix(3, ">")
                     .withInfix(3, "==")
                     .withInfix(0, ":=")
                     .withInfix(0, "++")
                     .withPostfix(1, "!")

  val code =
    """
    def +(a, b) =
      opcode %{
        load [I32] a
        load [I32] b
        add [I32]
      }

    def -(a, b) =
      opcode %{
        load [I32] a
        load [I32] b
        sub [I32]
      }

    def ++(a, b) =
      opcode %{
        load [Str] a
        load [Str] b
        concat
      }

    def println(x) =
      opcode %{
        load [Str] x
        println
        ret
      }

    // let
    //   ++ = func (a, b) = a + b
    //   cond = func (a, b) = a ++ b
    //   count_down = func (x) =
    //     if x
    //     then count_down(x - 1)
    //     else x
    //   something = (func (a) = println(a + a))(232)
    //   something_else =
    //     begin
    //       123
    //       321
    //       begin
    //         111
    //         111
    //       end
    //       begin
    //         111
    //         27
    //       end
    //     end
    //   a = 1+3
    //   b = func (x) = x
    //   c = func (x) = x+x
    //   d = if 0
    //       then 123
    //       else a
    //   e = func (x) =
    //         if x
    //         then x
    //         else x + 1
    //   f = func () =
    //         func () =
    //           func () =
    //             func () =
    //               func (x) = x + x
    // in begin
    //   begin
    //     if cond(0, 1)
    //     then 123
    //     else 1
    //   end
    //   begin
    //     println(f()()()()(4))
    //   end
    //   count_down(1000)
    //   count_down(1000)
    //   count_down(1000)
    //   // println(123)
    //   // println(321)
    //   1 + 3
    //   // println(22 + 20)
    //   println(something_else)
    // end
    //
    // def print_something = func (a) =
    //   let b = a + 1 in println(b)
    //
    // def x = 2
    // println(777)
    // println(x + x)
    // let y = x + x + x in println(y)
    //
    // def add (a, b) = a + b
    //
    // def add2 (a, b) =
    //   %{
    //     add [I32]
    //   }
    //
    // println(add2(1, 2))
    // println('testing)
    // println(%{testing 1 2 3})
    //
    // 'north
    // 'south
    // 'east
    // 'west
    //
    // let
    //   testtesttest = func (a) = println(a)
    //   internalprintln = func (str) = println(str)
    //   puts = internalprintln
    // in
    //   puts(123)
    //
    //def puts(str) = println(str)
    // def globalprintln(str) = println(str)
    // def puts = globalprintln
    // def puts2 = puts
    // def xxx = 123
    //
    // puts(%{from puts})
    // puts2(%{from puts2})
    // globalprintln(%{from internalprintln})
    // println(%{from println})
    // puts2(23 + 19)
    //
    // def res = add(1, x)
    // println(res)
    //
    // def to_s (n : I32) = dispatch('i32_to_str, n)
    // def to_s (n : I64) = dispatch('i64_to_str, n)
    // def to_s (n : U32) = dispatch('u32_to_str, n)
    // def to_s (n : U64) = dispatch('u64_to_str, n)
    // def to_s (n : F32) = dispatch('f32_to_str, n)
    // def to_s (n : F64) = dispatch('f64_to_str, n)
    //
    // TODO variable lookup is wrong and invalid namespacing is set when
    // generating instructions.
    // def global_add_em(a, b) = a + b
    // def global_add_em_2 = global_add_em
    //
    // let
    //   internal_add_em = func (a, b) = a + b
    //   internal_add_em_2 = internal_add_em
    // in
    //   global_add_em_2(internal_add_em_2(3, 4), 7)
    //
    // let
    //   a = 1
    // in
    //   let
    //     b = 2
    //   in
    //     a + b
    //
    // def x = 1
    //
    // def y = (
    //   func (a) =
    //     func (b) =
    //       func () =
    //         a + x + b)(3)(2)()
    //
    // println(y)
    //
    // let
    //   add = func (a, b) = a + b
    //   add2 = add
    // in
    //   println(add2(1, 9))
    //
    // def base = 1
    //
    // def start =
    //   func (a) =
    //     func (b) =
    //       func (c) =
    //         a + b + c + base
    //
    // def a = start(2)
    // def a_b = a(5)
    // def a_b_c = a_b(7) // 2 + 5 + 7 + 1 = 15
    //
    // def ab = start(2)(3)
    // def ab_c = ab(6) // 2 + 5 + 6 + 1 = 12
    //
    // def abc = start(2)(3)(4) // 2 + 3 + 4 + 1 = 10
    //
    // def res = a_b_c + ab_c + abc // 15 + 12 + 10 = 37
    //
    // println(res)
    //
    // begin
    // println(123)
    // println(1 + 4)
    // println(4 - 1)
    // println(%{one} ++ %{ two})
    // end
    //
    // def x = (func (a) = a + a)(123)
    // def y = x + x
    // def z = func (a) = a + y + x
    //
    // def aaa =
    //   func (a) =
    //     func (b) =
    //       func (c) =
    //         a + b + c
    //
    // def bbb = aaa(1)(2)
    //
    // println(if 1 then bbb(3) else bbb(10))
    //
    // begin
    //   let
    //     f = func () =
    //           func () =
    //             func () =
    //               func () =
    //                 func (x) = x + x
    //   in
    //     println(f()()()()(4))
    // end
    //
    // def x = 321
    //
    // println(x)
    //
    // let x = 123 in
    //   begin
    //     println(x)
    //
    //     let x = 456 in
    //       println(x)
    //
    //     println(x)
    //   end
    //
    // println(x)
    //
    // println(123)
    // println(%{testing 123 })
    //
    // def err_message = %{should not see this}
    // def ok_message = %{it's ok to see this}
    //
    // def jump_test_1() =
    //   begin
    //     opcode %{
    //       jmp jump_test_2_entry
    //     }
    //     println(err_message)
    //   end
    //
    // def jump_test_2() =
    //   opcode %{
    //     load [Ptr] err_message
    //     println
    //     halt
    //   jump_test_2_entry:
    //     load [Ptr] ok_message
    //     println
    //   }
    //
    // jump_test_1()

    def loop(times, fn) =
      if times
      then
        begin
          fn(times)
          loop(times - 1, fn)
        end
      else fn

    def f(i) =
      opcode %{
        load [Str] i
        println
        ret
      }

    loop(10, println)
    """

  // parsing.opcode.parse(
  //   """
  //   add [I32]
  //   """
  // ).map { tree => println(tree.nodes.head) }

  val res =
    for
      ast <- parse("<stdin>", code, syntax)
      // _=println(s"AST: ${ast}\n\n")
      ir = typeless.lift(ast)
      // _=println(s"IR: ${ir}\n\n")
      ins <- backend.opcode.compile(ir)
    yield
      println("==================")
      println(pp(ins))
      println("==================")

      val interpreter = Interpreter(ins)//.debugging//.stepping

      interpreter.run

      "ok"

  println(res)

  // import runtime.instruction._
  // println(backend.bytecode.generateLabel(Label("testing")))
