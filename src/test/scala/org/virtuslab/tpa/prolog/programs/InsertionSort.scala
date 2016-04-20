package org.virtuslab.tpa.prolog.programs

import org.virtuslab.tpa.prolog.AST._
import org.virtuslab.tpa.prolog.Interpreter.Sat

object InsertionSort extends ProgramTestHelper {
  override val program =
    """% test case: Insertion Sort
      |
      |[lt-n: a, b].
      |[lt-n: b, c].
      |[lt-n: c, d].
      |[lt-n: d, e].
      |
      |% transitive closure on lt-n relation
      |{([lt-n: #x, #y]) => [lt: #x, #y]}.
      |{([lt-n: #x, #y], [lt: #y, #z]) => [lt: #x, #z]}.
      |{([lt: #x, #y]) => [ge: #y, #x]}.
      |{(<#x = #y>) => [ge: #x, #y]}.
      |
      |% test
      |([lt: #x, #y])?
      |
      |% insertion sort
      |[sorted: nil, nil].
      |{([sorted: #t, #y], [insert: #h, #y, #z]) => [sorted: [cons: #h, #t], #z]}.
      |[insert: #h, nil, [cons: #h, nil]].
      |{([lt: #h, #h2]) => [insert: #h, [cons: #h2, #t], [cons: #h, [cons: #h2, #t]]]}.
      |{([ge: #h, #h2], [insert: #h, #t, #z]) => [insert: #h, [cons: #h2, #t], [cons: #h2, #z]]}.
      |
      |% test
      |([sorted: [cons: a, [cons: b, nil]], #z])?
      |([sorted: [cons: b, [cons: d, [cons: a, [cons: b, [cons: c, [cons: e, nil]]]]]], #z])?
      |
      |% all done here.
      |quit!""".stripMargin

  private val nil = Atom("nil")
  private val H = Var("h")
  private val H2 = Var("h2")

  private def ltn(t1: Term, t2: Term) = Rel("lt-n", List(t1, t2))
  private def lt(t1: Term, t2: Term) = Rel("lt", List(t1, t2))
  private def ge(t1: Term, t2: Term) = Rel("ge", List(t1, t2))
  private def sorted(t1: Term, t2: Term) = Rel("sorted", List(t1, t2))
  private def insert(t1: Term, t2: Term, t3: Term) = Rel("insert", List(t1, t2, t3))
  private def cons(t1: Term, t2: Term) = Rel("cons", List(t1, t2))

  override val syntaxTree = List(NoOp,
    NoOp,
    Fact(ltn(a, b)),
    Fact(ltn(b, c)),
    Fact(ltn(c, d)),
    Fact(ltn(d, e)),
    NoOp,
    NoOp,
    lt(X, Y) :- ltn(X, Y),
    lt(X, Z) :-(ltn(X, Y), lt(Y, Z)),
    ge(Y, X) :- lt(X, Y),
    ge(X, Y) :- (X =:= Y),
    NoOp,
    NoOp,
    lt(X, Y) ?,
    NoOp,
    NoOp,
    Fact(sorted(nil, nil)),
    sorted(cons(H, T), Z) :-(sorted(T, Y), insert(H, Y, Z)),
    Fact(insert(H, nil, cons(H, nil))),
    insert(H, cons(H2, T), cons(H, cons(H2, T))) :- lt(H, H2),
    insert(H, cons(H2, T), cons(H2, Z)) :-(ge(H, H2), insert(H, T, Z)),
    NoOp,
    NoOp,
    sorted(cons(a, cons(b, nil)), Z) ?,
    sorted(cons(b, cons(d, cons(a, cons(b, cons(c, cons(e, nil)))))), Z) ?,
    NoOp,
    NoOp,
    Quit)

  private val programStateAfterLine12 = populateFacts(syntaxTree take 12)

  override def testOutput: Seq[TestOutput] = Seq(
    TestOutput(programStateAfterLine12, lt(X, Y) ?, Sat(
      Seq(Seq(X -> a, Y -> b), Seq(X -> b, Y -> c), Seq(X -> c, Y -> d), Seq(X -> d, Y -> e), Seq(X -> a, Y -> c),
        Seq(X -> a, Y -> d), Seq(X -> a, Y -> e), Seq(X -> b, Y -> d), Seq(X -> b, Y -> e), Seq(X -> c, Y -> e)))),

    TestOutput(stateAtTheEOF, sorted(cons(a, cons(b, nil)), Z) ?, Sat(Seq(Seq(Z -> cons(a, cons(b, nil)))))),

    TestOutput(stateAtTheEOF, sorted(cons(b, cons(d, cons(a, cons(b, cons(c, cons(e, nil)))))), Z) ?,
      Sat(Seq(Seq(Z -> cons(a, cons(b, cons(b, cons(c, cons(d, cons(e, nil))))))))))
  )
}
