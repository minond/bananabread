package bananabread
package ir.typeless
package passes


def listReferencedIds(binding: Binding): Set[String] =
  listReferencedIds(binding.value)
def listReferencedIds(node: Ir): Set[String] =
  node match
    case _: Num    => Set.empty
    case _: Str    => Set.empty
    case _: Bool   => Set.empty
    case _: Symbol => Set.empty
    case _: Opcode => Set.empty
    case Lista(items, _) =>
      items.map(listReferencedIds).flatten.toSet
    case Id(name)  =>
      Set(name.lexeme)
    case App(Id(name), args, _) =>
      Set(name.lexeme) ++ args.map(listReferencedIds).flatten
    case App(lam, args, _) =>
      listReferencedIds(lam) ++ args.map(listReferencedIds).flatten
    case Def(_, body, _) =>
      listReferencedIds(body)
    case Lambda(_, body, _, _) =>
      listReferencedIds(body)
    case Begin(ins, _) =>
      ins.map(listReferencedIds).flatten.toSet
    case Cond(cond, pass, fail, _) =>
      List(cond, pass, fail).map(listReferencedIds).flatten.toSet
    case Let(bindings, body, _) =>
      listReferencedIds(body) ++ bindings.map(listReferencedIds).flatten


def listDefinedLambdas(node: Ir): Set[String] =
  node match
    case Def(name, _, _) => Set(name.lexeme)
    case _               => Set.empty
