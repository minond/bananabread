package bisquit
package parser

import scala.util.{Try, Success, Failure}

case class Syntax(uniops: Seq[String], binops: Seq[String]) {
  def isUniop(lexeme: String) = uniops.contains(lexeme)
  def isBinop(lexeme: String) = binops.contains(lexeme)
  def withUniop(lexeme: String) = Syntax(lexeme +: uniops, binops)
  def withBinop(lexeme: String) = Syntax(uniops, lexeme +: binops)
}

object Syntax {
  def withUniop(lexeme: String) = Syntax(Seq.empty, Seq.empty).withUniop(lexeme)
  def withBinop(lexeme: String) = Syntax(Seq.empty, Seq.empty).withBinop(lexeme)
}

def parse(sourceName: String, sourceString: String, syntax: Syntax): Either[ast.SyntaxErr, List[ast.Expr]] =
  tokenize(sourceName, sourceString).flatMap { tokens => parse(tokens, syntax) }

def parse(tokens: List[ast.Token], syntax: Syntax): Either[ast.SyntaxErr, List[ast.Expr]] =
  Right(List.empty)

def tokenize(sourceName: String, sourceString: String): Either[ast.SyntaxErr, List[ast.Token]] =
  val stream = sourceString.iterator.zipWithIndex.buffered
  squisherr(
    stream
      .filter { (c, _) => !c.isWhitespace }
      .map { (c, i) => nextToken(c, stream, ast.Location(sourceName, i)) }
      .toList)

def nextToken(head: Char, tail: BufferedIterator[(Char, Int)], loc: ast.Location): Either[ast.SyntaxErr, ast.Token] =
  head match {
    case ',' => Right(ast.Comma(loc))
    case '.' => Right(ast.Dot(loc))
    case ':' => Right(ast.Colon(loc))
    case '=' => Right(ast.Equal(loc))
    case '(' => Right(ast.OpenParen(loc))
    case ')' => Right(ast.CloseParen(loc))
    case '{' => Right(ast.OpenCurlyParen(loc))
    case '}' => Right(ast.CloseCurlyParen(loc))
    case '[' => Right(ast.OpenSquareBraket(loc))
    case ']' => Right(ast.CloseSquareBraket(loc))

    case head if isNumHead(head) =>
      val rest = takeWhile(tail, isNumTail).mkString
      val lexeme = head +: rest
      Try { lexeme.toFloat } match {
        case Failure(_) => Left(ast.BadNumErr(lexeme, loc))
        case Success(_) => Right(ast.Num(lexeme, loc))
      }

    case head if isIdHead(head) =>
      val rest = takeWhile(tail, isIdTail).mkString
      val lexeme = head +: rest
      Right(ast.Id(lexeme, loc))

    case _ => Left(ast.UnknownCharErr(head, loc))
  }


type Pred[T] = T => Boolean
type Preds[T] = Seq[Pred[T]]
type Predcond = (Boolean, Boolean) => Boolean

def flpreds[T](preds: Preds[T], id: Boolean = true)(cond: Predcond) =
  (c: T) => preds.foldLeft(id)((acc, pred) => cond(acc, pred(c)))
def pred[T]()(pred: => Pred[T]) =
  (c: T) => pred(c)

def ge[T <: Char](x: T) = (c: T) => c >= x
def le[T <: Char](x: T) = (c: T) => c <= x
def is[T <: Char](x: T) = (c: T) => c == x
def oneof[T <: Char](xs: T*) = (c: T) => xs.contains(c)
def not[T <: Char](f: Pred[T]) = flpreds(Seq(f))(_ && !_)
def and[T <: Char](fs: Pred[T]*) = flpreds(fs)(_ && _)
def or[T <: Char](fs: Pred[T]*) = flpreds(fs, false)(_ || _)

val isWhitespace = oneof(' ', '\t', '\r', '\n', '\f')
val isNumHead = and(ge('0'),
                    le('9'))
val isNumTail = or(isNumHead,
                   is('.'))
val isIdTail = and(not(isWhitespace),
                   not(oneof('(', ')', '{', '}', '[', ']', ',', '.', '=', ':')))
val isIdHead = and(isIdTail,
                   not(isNumTail))

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

def squisherr[L, R](eithers: List[Either[L, R]]): Either[L, List[R]] =
  eithers.foldLeft[Either[L, List[R]]](Right(List.empty)) { (acc, x) =>
    acc.flatMap(xs => x.map(xs :+ _))
  }
