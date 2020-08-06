package xyz.minond.bisquit.token

import xyz.minond.bisquit.input.Positioned

sealed trait Token extends Positioned
sealed trait Expression extends Token
sealed trait Value extends Expression

case object Eof extends Token
case class Id(lexeme: String) extends Expression
case class Binop(op: Id, left: Expression, right: Expression) extends Expression
case class Uniop(op: Id, subject: Expression) extends Expression
case class App(fn: Id | Func, args: List[Expression]) extends Expression
case class Num(value: Double) extends Value

case class Func(params: List[Id], body: Expression) extends Value {
  def curried(bindings: List[Value]) =
    Func(params=params.drop(bindings.size),
         body=App(Func(params.take(bindings.size), body), bindings))
}

case class Builtin(f: List[Value] => Value) extends Value {
  def apply(args: List[Value]) =
    f(args)
}

def numericBinaryBuiltin(f: (Double, Double) => Double): Builtin =
  Builtin({
    case Num(left) :: Num(right) :: Nil => Num(f(left, right))
  })

def numericUnaryBuiltin(f: Double => Double): Builtin =
  Builtin({
    case Num(right) :: Nil => Num(f(right))
  })
