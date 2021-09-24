package bananabread
package test

import org.scalatest._
import flatspec._
import matchers._


class ProgramTests extends AnyFlatSpec with should.Matchers:
  for path <- internalTestProgramPaths() do
    it should s"execute $path" in {
      val test = program.readFile(path)
      outputOf(test) shouldEqual expectedOutput(test)
    }
