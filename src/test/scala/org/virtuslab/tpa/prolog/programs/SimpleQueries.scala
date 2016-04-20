package org.virtuslab.tpa.prolog.programs

import org.virtuslab.tpa.prolog.AST._
import org.virtuslab.tpa.prolog.Interpreter.{Sat, Unsat}

object SimpleQueries extends ProgramTestHelper {
  override val program =
    """% Simple test case
      |% verifies the basic reasoning
      |
      |[r: a].
      |{([r: #z]) => [p: [r: #z], #z]}.
      |([p: [r: a], a])?
      |([p: #a, a])?
      |([p: #a, b])?
      |([p: [r: a], #a])?
      |([p: #a, #b])?
      |([p: #c, #c])?
      |([p: #a, #b], <#a = #b>)?
      |([p: #a, #b], <#a /= #b>)?
      |([p: #a, #b], <a /= #b>)?
      |
      |quit!""".stripMargin

  override val syntaxTree = List(NoOp,
    NoOp,
    NoOp,
    Fact(r(a)),
    p(r(Z), Z) :- r(Z),
    p(r(a), a) ?,
    p(A, a) ?,
    p(A, b) ?,
    p(r(a), A) ?,
    p(A, B) ?,
    p(C, C) ?,
    (p(A, B) :: (A =:= B) :: Nil) ?,
    (p(A, B) :: (A =/= B) :: Nil) ?,
    (p(A, B) :: (a =/= B) :: Nil) ?,
    NoOp,
    Quit)

  override def testOutput = Seq(
    TestOutput(stateAtTheEOF, p(r(a), a) ?, Sat(Nil)),
    TestOutput(stateAtTheEOF, p(A, a) ?, Sat(Seq(Seq(A -> r(a))))),
    TestOutput(stateAtTheEOF, p(A, b) ?, Unsat),
    TestOutput(stateAtTheEOF, p(r(a), A) ?, Sat(Seq(Seq(A -> a)))),
    TestOutput(stateAtTheEOF, p(A, B) ?, Sat(Seq(Seq(A -> r(a), B -> a)))),
    TestOutput(stateAtTheEOF, p(C, C) ?, Unsat),
    TestOutput(stateAtTheEOF, (p(A, B) :: (A =:= B) :: Nil) ?, Unsat),
    TestOutput(stateAtTheEOF, (p(A, B) :: (A =/= B) :: Nil) ?, Sat(Seq(Seq(A -> r(a), B -> a)))),
    TestOutput(stateAtTheEOF, (p(A, B) :: (a =/= B) :: Nil) ?, Unsat))

  private def r(t: Term) = Rel("r", List(t))
  private def p(t1: Term, t2: Term) = Rel("p", List(t1, t2))
}
