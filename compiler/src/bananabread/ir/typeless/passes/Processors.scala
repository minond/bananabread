package bananabread
package ir.typeless
package passes


def listCalledLambdas(binding: Binding): Set[String] =
  listCalledLambdas(binding.value)
def listCalledLambdas(node: Ir): Set[String] =
  node match
    case _: Num    => Set.empty
    case _: Str    => Set.empty
    case _: Bool   => Set.empty
    case _: Symbol => Set.empty
    case Id(name)  =>
      Set(name.lexeme)
    case App(Id(name), args, _) =>
      Set(name.lexeme) ++ args.map(listCalledLambdas).flatten
    case App(lam, args, _) =>
      listCalledLambdas(lam) ++ args.map(listCalledLambdas).flatten
    case Def(_, body, _) =>
      listCalledLambdas(body)
    case Lambda(_, body, _, _) =>
      listCalledLambdas(body)
    case Begin(ins, _) =>
      ins.map(listCalledLambdas).flatten.toSet
    case Cond(cond, pass, fail, _) =>
      List(cond, pass, fail).map(listCalledLambdas).flatten.toSet
    case Let(bindings, body, _) =>
      listCalledLambdas(body) ++ bindings.map(listCalledLambdas).flatten


def listDefinedLambdas(node: Ir): Set[String] =
  node match
    case Def(name, _, _) => Set(name.lexeme)
    case _               => Set.empty
