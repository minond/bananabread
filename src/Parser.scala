package bisquit
package parser

import scala.util.{Try, Success, Failure}


def parse(sourceName: String, string: String): Iterator[Either[ast.SyntaxErr, ast.Token]] =
  val stream = string.iterator.zipWithIndex.buffered
  for
    (head, offset) <- stream if !head.isWhitespace
  yield
    nextToken(head, stream, ast.Location(sourceName, offset))

def nextToken(head: Char, tail: BufferedIterator[(Char, Int)], loc: ast.Location): Either[ast.SyntaxErr, ast.Token] =
  head match {
    case head if isNumHead(head) =>
      val rest = takeWhile(tail, isNumTail).mkString
      val lexeme = head +: rest
      Try { lexeme.toFloat } match {
        case Failure(_) => Left(ast.BadNumErr(lexeme, loc))
        case Success(_) => Right(ast.Num(lexeme, loc))
      }
  }


type Pred[T] = T => Boolean
type Predbiop[T] = (T, T) => Boolean
type Preds[T] = Seq[Pred[T]]
type Predcond = (Boolean, Boolean) => Boolean

def flpreds[T](preds: Preds[T], id: Boolean = true)(cond: Predcond) =
  (c: T) => preds.foldLeft(id)((acc, pred) => cond(acc, pred(c)))
def bipred[T](x: T)(pred: Predbiop[T]) =
  (c: T) => pred(c, x)

def ge[T <: Char](x: T) = bipred(x)(_ >= _)
def le[T <: Char](x: T) = bipred(x)(_ <= _)
def is[T <: Char](x: T) = bipred(x)(_ == _)
def not[T <: Char](f: Pred[T]) = flpreds(Seq(f))(_ && !_)
def and[T <: Char](fs: Pred[T]*) = flpreds(fs)(_ && _)
def or[T <: Char](fs: Pred[T]*) = flpreds(fs)(_ || _)

val isNumHead = and(ge('0'), le('9'))
val isNumTail = or(isNumHead, is('.'))

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
