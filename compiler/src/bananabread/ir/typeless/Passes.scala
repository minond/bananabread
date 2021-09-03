package bananabread
package ir.typeless

import passes.dropDeadCode._


val Passes = List(
  dropUnnecessaryLambdas,
)


def pass(nodes: List[Ir]): List[Ir] =
  Passes.foldLeft(nodes) { (nodes, pass) => pass(nodes) }
