package bananabread
package parsing
package program

import ast._


def structure(tree: Tree): (Option[Module], Tree) =
  tree.nodes.foldLeft[(Option[Module], Tree)]((None, Tree.empty)) {
    case ((_, tree), module: Module) =>
      (Some(module), tree)
    case ((module, Tree(nodes)), node) =>
      (module, Tree(nodes :+ node))
  }
