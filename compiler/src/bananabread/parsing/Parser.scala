package bananabread
package parsing.parser

import parsing.ast.{Token, Tree, Expr, Stmt, Eof, Id}
import parsing.error._

import scala.reflect.ClassTag


type TokenBuffer = BufferedIterator[Token]

// Parser predicates and combinators

type Pred[T] = T => Boolean
type Predcond = (Boolean, Boolean) => Boolean

def flpreds[T](preds: Seq[Pred[T]], id: Boolean = true)(cond: Predcond) =
  (c: T) => preds.foldLeft(id)((acc, pred) => cond(acc, pred(c)))

def ge[T <: Char](x: T) = (c: T) => c >= x
def le[T <: Char](x: T) = (c: T) => c <= x
def is[T <: Char](x: T) = (c: T) => c == x
def oneof[T <: Char](xs: T*) = (c: T) => xs.contains(c)
def not[T <: Char](f: Pred[T]) = flpreds(Seq(f))(_ && !_)
def and[T <: Char](fs: Pred[T]*) = flpreds(fs)(_ && _)
def or[T <: Char](fs: Pred[T]*) = flpreds(fs, false)(_ || _)

val isNewline = oneof('\r', '\n')
val isWhitespace = oneof(' ', '\t', '\r', '\n', '\f')
val isLetter = or(and(ge('a'), le('z')),
                  and(ge('A'), le('Z')))
val isNumeric = and(ge('0'),
                    le('9'))


// Predicate/combinator stream processors

def takeWhile[T](source: BufferedIterator[(T, _)], pred: Pred[T]): List[T] =
  def aux(buff: List[T]): List[T] =
    if source.isEmpty
    then buff
    else
      val curr = source.head
      if pred(curr._1)
      then aux(buff :+ source.next._1)
      else buff
  aux(List.empty)

def skip[T](it: BufferedIterator[T]): BufferedIterator[T] =
  it.next
  it

def eat[T: ClassTag](head: Token, tail: TokenBuffer): Either[SyntaxErr, T] =
  tail.headOption match
    case Some(token: T) =>
      tail.next
      Right(token)

    case Some(unexpected) => Left(UnexpectedTokenErr[T](unexpected))
    case None => Left(UnexpectedEofErr(head))

def eat(word: String, head: Token, tail: TokenBuffer): Either[SyntaxErr, Token] =
  tail.headOption match
    case Some(id: Id) if is(id, word) =>
      tail.next
      Right(id)

    case Some(unexpected) => Left(UnexpectedTokenErr(unexpected))
    case None => Left(UnexpectedEofErr(head))

def lookahead(head: Token, tail: TokenBuffer): Token =
  tail.headOption match
    case None => Eof(head.location)
    case Some(token) => token

def is(token: Token, word: String) = token match
  case id: Id => id.lexeme == word
  case _ => false
