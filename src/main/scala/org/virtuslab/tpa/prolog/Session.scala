package org.virtuslab.tpa.prolog

object Session {
  import AST._

  def main(args: Array[String]): Unit = {
    val in = io.Source.stdin.getLines()

    import Parser._
    val input = in map parseLine takeWhile {
      case Success(Quit, _) => false
      case _ => true
    }
    (Interpreter() /: input) ((interpreter, input) => input match {
      case Success(op, _) => interpreter eval op
      case NoSuccess(msg, _) => println(msg); interpreter
    })

    println("Bye.")
  }
}
