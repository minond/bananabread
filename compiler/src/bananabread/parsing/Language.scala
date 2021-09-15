package bananabread
package parsing
package language

import location.Location
import ast._
import error._
import utils.{isAn, squished, safeToFloat, without}

import scala.reflect.ClassTag


type Tokens = BufferedIterator[Token]

def parse(sourceName: String, sourceString: String): Parsed[Tree] =
  parse(sourceName, sourceString, Syntax())
def parse(sourceName: String, sourceString: String, syntax: Syntax): Parsed[Tree] =
  tokenize(sourceName, sourceString, syntax).flatMap { tokens =>
    parse(sourceName, tokens.without[Comment].iterator.buffered, syntax)
  }
def parse(sourceName: String, tokens: Tokens, syntax: Syntax): Parsed[Tree] =
  for
    nodes <- tokens
      .foldLeft[(List[Parsed[Stmt | Expr]], Syntax)](List.empty, syntax) {
        case ((acc, syntax), token) =>
          val expr = parseTop(token, tokens, syntax)
          if ! isOperatorDefinition(expr)
          then (acc :+ expr, syntax)
          else operatorDefinition(expr) match
            case Some(pos, prec, name) =>
              (acc, syntax.withOp(pos, prec, name))
            case None =>
              (acc :+ Left(BadOperatorDefinitionErr(token.location)), syntax)
      }
      ._1
      .squished
  yield
    Tree(nodes)

def parseTop(head: Token, tail: Tokens, syntax: Syntax): Parsed[Stmt | Expr] =
  head match
    case _ if is(head, "def") => parseDef(head, tail, syntax)
    case _ => parseExpr(head, tail, syntax)

def parseExpr(head: Token, tail: Tokens, syntax: Syntax): Parsed[Expr] =
  head match
    case op: Id if syntax.isPrefix(op) => parseExprCont(parseUniop(op, tail, syntax), tail, syntax)
    case _ => parseExprCont(parsePrimary(head, tail, syntax), tail, syntax)

def parseUniop(op: Id, tail: Tokens, syntax: Syntax): Parsed[Uniop] =
  for rhs <- parsePrimary(tail.next, tail, syntax)
  yield Uniop(op, rhs)

def parsePrimary(head: Token, tail: Tokens, syntax: Syntax): Parsed[Expr] =
  head match
    case word: Id if is(word, "func")  => parseLambda(word, tail, syntax)
    case word: Id if is(word, "if")    => parseCond(word, tail, syntax)
    case word: Id if is(word, "let")   => parseLet(word, tail, syntax)
    case word: Id if is(word, "begin") => parseBegin(word, tail, syntax)
    case head: OpenParen               => parseGroup(head, tail, syntax)
    case head: Percent                 => parseSpecial(head, tail, syntax)
    case lit: Num    => Right(lit)
    case lit: Str    => Right(lit)
    case lit: Id     => Right(lit)
    case lit: Symbol => Right(lit)
    case lit: Bool   => Right(lit)
    case unexpected => Left(UnexpectedTokenErr(unexpected))

def parseLambda(start: Token, tail: Tokens, syntax: Syntax): Parsed[Lambda] =
  for
    tyVars <- if lookahead(start, tail).isAn[OpenSquareBraket]
              then parseByUntilWith[Comma, CloseSquareBraket, TyId](tail.next, tail, syntax, parseTyVar)
              else Right(List.empty)
    params <- parseParams(tail.next, tail, syntax)
    tyRet  <- parseOptionalTy(start, tail, syntax)
    eq     <- eat("=", start, tail)
    body   <- parseNext(eq, tail, syntax, parseExpr)
  yield
    Lambda(params, body, tyVars, tyRet)

def parseParams(
  head: Token,
  tail: Tokens,
  syntax: Syntax,
  acc: List[Param] = List.empty
): Parsed[List[Param]] =
  tail.headOption match
    case Some(_: CloseParen) =>
      tail.next
      Right(acc)

    case Some(by: Comma) =>
      parseParam(tail.next, tail, syntax).flatMap { param =>
        parseParams(head, tail, syntax, acc :+ param)
      }

    case Some(_) =>
      parseParam(head, tail, syntax).flatMap { param =>
        parseParams(head, tail, syntax, acc :+ param)
      }

    case None =>
      Left(UnexpectedEofErr(head))

def parseParam(head: Token, tail: Tokens, syntax: Syntax): Parsed[Param] =
  eat[Id](head, tail).flatMap { name =>
    parseOptionalTy(name, tail, syntax).map { ty =>
      Param(name, ty)
    }
  }

def parseOptionalTy(head: Token, tail: Tokens, syntax: Syntax): Parsed[Option[Ty]] =
  tail.headOption match
    case Some(_: Colon) =>
      parseTy(tail.next, tail, syntax).map(Some(_))
    case _ =>
      Right(None)

def parseTy(head: Token, tail: Tokens, syntax: Syntax): Parsed[Ty] =
  eat[Id](head, tail).flatMap { id =>
    if lookahead(id, tail).lexemeIs("->")
    then // T -> R
      for
        retTy <- parseTy(tail.next, tail, syntax)
      yield
        TyLamda(List(TyId(id)), retTy)
    else // T
      Right(TyId(id))
  }

/** TODO This needs to be able to parse complex type expression, like `X < Y`.
  */
def parseTyVar(head: Token, tail: Tokens, syntax: Syntax): Parsed[TyId] =
  head match
    case id: Id => Right(TyId(id))
    case _      => Left(UnexpectedTokenErr(head))

def parseCond(start: Token, tail: Tokens, syntax: Syntax): Parsed[Cond] =
  for
    cond <- parseExpr(tail.next, tail, syntax)
    _    <- eat("then", start, tail)
    pass <- parseExpr(tail.next, tail, syntax)
    _    <- eat("else", start, tail)
    fail <- parseExpr(tail.next, tail, syntax)
  yield
    Cond(start, cond, pass, fail)

def parseLet(start: Token, tail: Tokens, syntax: Syntax): Parsed[Let] =
  for
    bindings <- parseBindings(start, tail, syntax)
    _        <- eat("in", start, tail)
    body     <- parseExpr(tail.next, tail, syntax)
  yield
    Let(start, bindings, body)

def parseDef(start: Token, tail: Tokens, syntax: Syntax): Parsed[Def] =
  for
    name  <- eat[Id](start, tail)
    next  = lookahead(start, tail)
    value <- if next.isAn[OpenParen] || next.isAn[OpenSquareBraket]
             then parseLambda(start, tail, syntax)
             else parseDefValue(start, tail, syntax)
  yield
    Def(name, value)

def parseDefValue(start: Token, tail: Tokens, syntax: Syntax): Parsed[Expr] =
  for
    _     <- eat("=", start, tail)
    value <- parseExpr(tail.next, tail, syntax)
  yield
    value

def parseBindings(start: Token, tail: Tokens, syntax: Syntax): Parsed[List[Binding]] =
  for
    binding  <- parseBinding(start, tail, syntax)
    next     = lookahead(start, tail)
    bindings <- if is(next, "in")
                then Right(List.empty)
                else parseBindings(start, tail, syntax)
  yield
    binding +: bindings

def parseBinding(start: Token, tail: Tokens, syntax: Syntax): Parsed[Binding] =
  for
    label <- eat[Id](start, tail)
    eq    <- eat("=", label, tail)
    value <- parseExpr(tail.next, tail, syntax)
  yield
    Binding(label, value)

def parseBegin(start: Token, tail: Tokens, syntax: Syntax): Parsed[Begin] =
  for
    heade <- if is(lookahead(start, tail), "end")
             then Left(EmptyBeginNotAllowedErr(start))
             else parseExpr(tail.next, tail, syntax)
    taile <- parseBeginTail(start, tail, syntax)
    _     <- eat("end", start, tail)
  yield
    Begin(heade, taile)

def parseBeginTail(start: Token, tail: Tokens, syntax: Syntax): Parsed[List[Expr]] =
  for
    heade <- if is(lookahead(start, tail), "end")
            then return Right(List.empty)
            else parseExpr(tail.next, tail, syntax)
    next  = lookahead(start, tail)
    taile <- if is(next, "end")
            then Right(List.empty)
            else parseBeginTail(start, tail, syntax)
  yield
    heade +: taile

def parseGroup(paren: Token, tail: Tokens, syntax: Syntax): Parsed[Expr] =
  for
    inner <- parseExpr(tail.next, tail, syntax)
    _     <- eat[CloseParen](paren, tail)
  yield
    inner

def parseSpecial(head: Token, tail: Tokens, syntax: Syntax): Parsed[Expr] =
  lookahead(head, tail) match
    case _: OpenSquareBraket =>
      eat[OpenSquareBraket](tail.next, tail).flatMap { head =>
        for
          inst <- parseUntil[CloseSquareBraket, opcode.Expr](head, tail, opcode.parseOpcodeInstruction)
          _    <- eat[CloseSquareBraket](head, tail)
          _    <- eat[CloseSquareBraket](head, tail)
        yield
          Opcode(inst, head.location)
      }

    case unexpected =>
      Left(UnexpectedTokenErr(unexpected))

def parseExprCont(currRes: Parsed[Expr], tail: Tokens, syntax: Syntax): Parsed[Expr] =
  currRes.flatMap { curr => parseExprCont(curr, tail, syntax) }
def parseExprCont(curr: Expr, tail: Tokens, syntax: Syntax): Parsed[Expr] =
  tail.headOption match
    case Some(op: Id) if syntax.isPostfix(op) =>
      tail.next
      Right(Uniop(op, curr))

    case Some(op: Id) if syntax.isInfix(op) =>
      for rhs <- parseNext(op, skip(tail), syntax, parseExpr)
      yield
        rhs match
          case Binop(nextOp, nextLhs, nextRhs) if syntax.isInfix(nextOp) &&
            syntax.infixPrecedence(op) >= syntax.infixPrecedence(nextOp) =>
              Binop(nextOp, Binop(op, curr, nextLhs), nextRhs)

          case _ => Binop(op, curr, rhs)

    case Some(paren: OpenParen) =>
      parseExprCont(parseByUntilWith[Comma, CloseParen, Expr](paren, skip(tail), syntax, parseExpr).map { args =>
        App(curr, args)
      }, tail, syntax)

    case Some(_) => Right(curr)
    case None => Right(curr)


def tokenize(sourceName: String, sourceString: String, syntax: Syntax): Parsed[List[Token]] =
  tokenize(sourceName, sourceString.iterator.zipWithIndex.buffered, syntax)
def tokenize(sourceName: String, sourceStream: BufferedIterator[(Char, Int)], syntax: Syntax): Parsed[List[Token]] =
  sourceStream
    .filter { (c, _) => !c.isWhitespace }
    .map { (c, i) => nextToken(c, sourceStream, Location(sourceName, i), syntax) }
    .squished

def nextToken(
  head: Char,
  tail: BufferedIterator[(Char, Int)],
  loc: Location,
  syntax: Syntax,
  ignoreComment: Boolean = false,
  ignorePString: Boolean = false,
): Parsed[Token] = head match
  case ',' => Right(Comma(loc))
  case '.' => Right(Dot(loc))
  case ':' => Right(Colon(loc))
  case '(' => Right(OpenParen(loc))
  case ')' => Right(CloseParen(loc))
  case '{' => Right(OpenCurlyParen(loc))
  case '}' => Right(CloseCurlyParen(loc))
  case '[' => Right(OpenSquareBraket(loc))
  case ']' => Right(CloseSquareBraket(loc))

  case '/' if !ignoreComment =>
    tail.headOption match
      case Some('/', _) =>
        val comment = takeWhile(skip(tail), not(isNewline))
        Right(Comment(comment.mkString.strip, loc))
      case _ => nextToken(head, tail, loc, syntax, ignoreComment=true)

  case '%' if !ignorePString =>
    tail.headOption match
      case Some('{', _) =>
        val str = takeWhile(skip(tail), aint('}'))
        if tail.isEmpty || tail.next._1 != '}'
        then Left(UnclosedStringErr(loc))
        else Right(Str(str.mkString, loc))
      case Some('[', _) =>
        Right(Percent(loc))
      case _ => nextToken(head, tail, loc, syntax, ignorePString=true)

  case '"' =>
    val str = takeWhile(tail, aint('"'))
    if tail.isEmpty || tail.next._1 != '"'
    then Left(UnclosedStringErr(loc))
    else Right(Str(str.mkString, loc))

  case '\'' =>
    val symbol = takeWhile(tail, isSymbolTail).mkString

    Right(Symbol(symbol, loc))

  case head if isNumHead(head) =>
    val rest = takeWhile(tail, isNumTail).mkString
    val lexeme = head +: rest

    lexeme.safeToFloat match
      case Left(_)  => Left(BadNumErr(lexeme, loc))
      case Right(_) => Right(Num(lexeme, loc))

  case head if isIdHead(head) =>
    val rest = takeWhile(tail, isIdTail).mkString
    val lexeme = head +: rest

    lexeme match
      case "true"  => Right(True(loc))
      case "false" => Right(False(loc))
      case id      => Right(Id(lexeme, loc))

  case head =>
    val rest = takeWhile(tail, isUnknownTail).mkString
    val lexeme = head +: rest

    Right(Id(lexeme, loc))


val isNumHead = isNumeric
val isNumTail = or(isNumHead,
                   is('.'))
val isIdTail = and(not(isWhitespace),
                   or(isNumeric,
                      isLetter,
                      is('_')))
val isIdHead = and(isIdTail,
                   not(isNumeric))
val isUnknownTail = and(not(isIdTail),
                        not(isWhitespace),
                        not(oneof(',', '.', '(', ')', '{', '}', '[', ']')))
val isSymbolTail = and(not(isWhitespace),
                       not(oneof(',', '(', ')', '{', '}', '[', ']')))

def isOperatorDefinition(expr: Parsed[Expr | Stmt]) = expr match
  case Right(App(Id("operator", _), _)) => true
  case _ => false

def operatorDefinition(expr: Parsed[Expr | Stmt]): Option[(OpPosition, Int, String)] = expr match
  case Right(App(Id("operator", _), Id("prefix",  _) :: Num(prec, _) :: Symbol(name, _) :: Nil)) => Some((Prefix,  prec.toInt, name))
  case Right(App(Id("operator", _), Id("prefix",  _) :: Num(prec, _) :: Id(name, _) :: Nil))     => Some((Prefix,  prec.toInt, name))
  case Right(App(Id("operator", _), Id("infix",   _) :: Num(prec, _) :: Symbol(name, _) :: Nil)) => Some((Infix,   prec.toInt, name))
  case Right(App(Id("operator", _), Id("infix",   _) :: Num(prec, _) :: Id(name, _) :: Nil))     => Some((Infix,   prec.toInt, name))
  case Right(App(Id("operator", _), Id("postfix", _) :: Num(prec, _) :: Symbol(name, _) :: Nil)) => Some((Postfix, prec.toInt, name))
  case Right(App(Id("operator", _), Id("postfix", _) :: Num(prec, _) :: Id(name, _) :: Nil))     => Some((Postfix, prec.toInt, name))
  case _                                                                                         => None
