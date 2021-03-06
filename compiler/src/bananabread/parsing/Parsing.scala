package bananabread
package parsing

import ast.{Token, Tree, Expr, Stmt, Eof, Id}
import error._

import utils.squished

import scala.reflect.ClassTag


type Tokens = BufferedIterator[Token]
type Parsed[T] = Either[SyntaxErr, T]

// Parser predicates and combinators

type Pred[T] = T => Boolean
type Predcond = (Boolean, Boolean) => Boolean

def flpreds[T](preds: Seq[Pred[T]], id: Boolean = true)(cond: Predcond) =
  (c: T) => preds.foldLeft(id)((acc, pred) => cond(acc, pred(c)))

def ge[T <: Char](x: T) = (c: T) => c >= x
def le[T <: Char](x: T) = (c: T) => c <= x
def is[T <: Char](x: T) = (c: T) => c == x
def aint[T <: Char](x: T) = (c: T) => c != x
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

def eat[T: ClassTag](head: Token, tail: Tokens): Parsed[T] =
  tail.headOption match
    case Some(token: T) =>
      tail.next
      Right(token)

    case Some(unexpected) => Left(UnexpectedTokenErr[T](unexpected))
    case None => Left(UnexpectedEofErr(head))

def eat(word: String, head: Token, tail: Tokens): Parsed[Token] =
  tail.headOption match
    case Some(id: Id) if is(id, word) =>
      tail.next
      Right(id)

    case Some(unexpected) => Left(UnexpectedTokenErr(unexpected))
    case None => Left(UnexpectedEofErr(head))

def next(head: Token, tail: Tokens): Token =
  tail.headOption match
    case Some(token) => tail.next
    case None        => Eof(head.location)

def lookahead(head: Token, tail: Tokens): Token =
  tail.headOption match
    case None => Eof(head.location)
    case Some(token) => token

def is(token: Token, word: String) = token match
  case id: Id => id.lexeme == word
  case _ => false


def parseByWith[By: ClassTag, T](
  head: Token,
  tail: Tokens,
  fn: (Token, Tokens) => Parsed[T],
  acc: List[T] = List.empty
): Parsed[List[T]] =
  fn(head, tail).flatMap { item =>
    lookahead(head, tail) match
      case _: By =>
        parseByWith(tail.next, tail, fn, acc :+ item)
      case _ =>
        Right(acc :+ item)
  }

def parseUntil[Until: ClassTag, T](
  head: Token,
  tail: Tokens,
  fn: (Token, Tokens) => Parsed[T]
): Parsed[List[T]] =
  def aux(acc: List[Parsed[T]]): List[Parsed[T]] =
    lookahead(head, tail) match
      case x: Until            => acc
      case _ =>
        fn(head, tail) match
          case res @ Left(err) => acc :+ res
          case res @ Right(ok) => aux(acc :+ res)

  aux(List.empty[Parsed[T]]).squished

def parseByUntilWith[By: ClassTag, Until: ClassTag, T](
  head: Token,
  tail: Tokens,
  syntax: Syntax,
  fn: (Token, Tokens, Syntax) => Parsed[T],
  acc: List[T] = List.empty
): Parsed[List[T]] =
  tail.headOption match
    case Some(_: Until) =>
      tail.next
      Right(acc)

    case Some(by: By) =>
      skip(tail).headOption match
        case None    => Left(UnexpectedEofErr(head))
        case Some(_) =>
          fn(tail.next, tail, syntax).flatMap { t =>
            parseByUntilWith[By, Until, T](head, tail, syntax, fn, acc :+ t)
          }

    case Some(_) =>
      fn(tail.next, tail, syntax).flatMap { t =>
        parseByUntilWith[By, Until, T](head, tail, syntax, fn, acc :+ t)
      }

    case None =>
      Left(UnexpectedEofErr(head))

def parseNext[T](
  head: Token,
  tail: Tokens,
  syntax: Syntax,
  fn: (Token, Tokens, Syntax) => Parsed[T],
): Parsed[T] =
  tail.headOption match
    case Some(_) => fn(tail.next, tail, syntax)
    case None    => Left(UnexpectedEofErr(head))


extension (token: Token)
  def lexemeIs(lexeme: String): Boolean =
    token match
      case Id(id, _) => id == lexeme
      case _ => false
