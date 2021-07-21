package bananabread

import parsing.language.{tokenize, parse}
import ir.typeless
import error.Errors
import error.pp => errpp
import runtime.Interpreter
import runtime.instruction.pp


def main(args: Array[String]) =
  val code =
    """
    operator('prefix, 1, 'opcode)
    operator('prefix, 1, '!)
    operator('prefix, 1, '∀)

    operator('infix, 0, ':=)
    operator('infix, 0, '_o_)
    operator('infix, 0, '|>)
    operator('infix, 2, '+)
    operator('infix, 2, '++)
    operator('infix, 2, '-)
    operator('infix, 2, ':)
    operator('infix, 3, '%)
    operator('infix, 3, '*)
    operator('infix, 3, '/)
    operator('infix, 3, '==)
    operator('infix, 3, '>)
    operator('infix, 4, '^)
    operator('infix, 10, '∈)
    operator('infix, 99, '^)

    operator('postfix, 1, '!)


    def +(a: I32, b: I32): I32 =
      opcode %{
        load [I32] a
        load [I32] b
        add [I32]
      }

    def -(a: I32, b: I32): I32 =
      opcode %{
        load [I32] a
        load [I32] b
        sub [I32]
      }

    def ++(a: Str, b: Str): Str =
      opcode %{
        load [Str] a
        load [Str] b
        concat
      }

    def println[T](x: T): Str =
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
    // def puts(str) = println(str)
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

    println(%{loop(10, f)})
    loop(4 + 6, f)
    println(%{loop(10, println)})
    loop(17 - 7, println)

    def adder1(a, b) =
      let res = a + b in res

    def adder2(a, b) =
      let adder = adder1 in adder(a, b)

    def adder3(a, b) =
      let
        x = a
        y = b
        z = x + y
      in z

    println(adder1(12, 3))
    println(adder2(12, 4))
    println(adder3(12, 5))

    let
      f = func () =
            func () =
              func () =
                func () =
                  func (x) =
                    func (y) =
                      x + y
      call_with = func (f, arg) =
        f(arg)
    in
      println(call_with(f()()()()(3), 2))
      // println(call_with(3, 2))

    def add2(a: I32, b: I32): I32 =
      a + b

    println(add2(4, 2))
    """

  // parsing.opcode.parse(
  //   """
  //   add [I32]
  //   """
  // ).map { tree => println(tree.nodes.head) }

  val res =
    for
      ast <- parse("<stdin>", code)
      // _ = println(s"AST: ${ast}\n\n")
      ir = typeless.lift(ast)
      // _ = println(s"IR: ${ir}\n\n")
      ins <- backend.opcode.compile(ir)
      // _ = println("==================")
      // _ = println(pp(ins))
      // _ = println("==================")
      interpreter = Interpreter(ins)//.debugging//.stepping
      _ <- interpreter.run
    yield
      "ok"

  res match
    case Right(msg) => println(msg)
    case Left(err: Errors) => println(errpp(err, code))
    case Left(err) => println(s"unhandled error: $err")
