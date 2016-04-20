package org.virtuslab.tpa.prolog

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import org.virtuslab.tpa.prolog.AST.Op
import org.virtuslab.tpa.prolog.programs._

class ParserTest extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val programs = Table(
    ("program", "syntax tree"),
    (SimpleQueries.program, SimpleQueries.syntaxTree),
    (MurderMystery.program, MurderMystery.syntaxTree),
    (InsertionSort.program, InsertionSort.syntaxTree)
  )

  property("parse correct programs") {
    forAll(programs) { (program: String, expectedSyntaxTree: List[Op]) =>
      val ops = program.lines.map(Parser.parseLine).toList

      every(ops) shouldBe a[Parser.Success[_]]

      val parsedSyntaxTree = ops.collect { case Parser.Success(op, _) => op }
      parsedSyntaxTree should be(expectedSyntaxTree)
    }
  }
}
