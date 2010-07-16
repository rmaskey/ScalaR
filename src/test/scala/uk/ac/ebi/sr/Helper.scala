package uk.ac.ebi.sr

import interpreter.{Interpreter, Expression, RParser}
import model.Sequential

/**
 *
 * Date: Jul 16, 2010
 * @author Taalai Djumabaev
 */

object Helper {

  def evaluate(input: String): Any = {
    RParser.parseUnwrap(input) match {
      case e: Expression => Interpreter.interpret(e)._1
      case _ => None
    }
  }


  def equalSeq[A, B](a: Sequential[A], b: Sequential[B]): Boolean = {
    if (a == b) return true
    if (a.length != b.length) return false
    for ((i, j) <- a.s.zip(b.s)) if (i != j) return false
    a.attributes == b.attributes
  }
}