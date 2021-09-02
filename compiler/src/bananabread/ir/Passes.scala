package bananabread
package ir.typeless

import ir.passes.dropDeadCode._


val passes = List(
  dropUnnecessaryLambdas,
)


def pass(nodes: List[Ir]): List[Ir] =
  passes.foldLeft(nodes) { (nodes, pass) => pass(nodes) }
