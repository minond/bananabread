package bananabread
package test

import org.scalatest._
import flatspec._
import matchers._


class ProgramTests extends AnyFlatSpec with should.Matchers:
  for program <- internalTestProgramPaths() do
    it should s"execute $program" in {
      val test = module.readFile(program)
      outputOf(test) shouldEqual expectedOutput(test)
    }
