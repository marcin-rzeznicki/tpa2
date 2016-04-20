package org.virtuslab.tpa.prolog

import scala.collection.GenTraversableOnce

object TermImplicits {
  import AST.{ComplexTerm, Var}

  implicit class Terms(val self: GenTraversableOnce[ComplexTerm]) extends AnyVal {
    def freeVars: Set[Var] = (Set.empty[Var] /: self) (_ ++ _.vars)
  }

}
