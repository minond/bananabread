package bananabread
package parsing
package program

import ast._


def structure(tree: Tree): (Option[Module], List[Import], Tree) =
  tree.nodes.foldLeft[(Option[Module], List[Import], Tree)]((None, List.empty, Tree.empty)) {
    case ((_, imports, tree), stmt: Module) =>
      (Some(stmt), imports, tree)
    case ((module, imports, tree), stmt: Import) =>
      (module, imports :+ stmt, tree)
    case ((module, imports, Tree(nodes)), node) =>
      (module, imports, Tree(nodes :+ node))
  }
