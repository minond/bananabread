package bananabread
package ir.typeless
package passes
package dropDeadCode


// TODO This needs to take scoping into account. As is, this will keep a
// top-level function that is not called when a scoped function with the same
// name is being called. Among other things, this means recursive functions are
// always kept.
//
// TODO This is currently keeping lambdas that are only called in other lambdas
// that are dropped. These are also unnecessary lambdas that need to be
// dropped.
def dropUnnecessaryLambdas(nodes: List[Ir]): List[Ir] =
  val called = nodes.foldLeft(Set.empty[String]) { (acc, node) =>
    acc ++ listReferencedIds(node)
  }

  val defined = nodes.foldLeft(Set.empty[String]) { (acc, node) =>
    acc ++ listDefinedLambdas(node)
  }

  val unnecessary = defined.diff(called)

  nodes.foldLeft(List.empty[Ir]) {
    case (nodes, Def(name, _: Lambda, _)) if unnecessary.contains(name.lexeme) =>
      nodes
    case (nodes, node) =>
      nodes :+ node
  }
