package uk.ac.ebi.sr
package interpreter

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 *
 * Date: 01.06.2010
 * @author Taalai Djumabaev
 */

@RunWith(classOf[JUnitRunner])
class InterpreterSuite extends FunSuite {
  def evaluate(input: String): Any = {
    RParser.parseUnwrap(input) match {
      case e: Expression => Interpreter.interpret(e)._1
              case _ => None
    }
  }

  test("argument matching mechanism with environment bindings test ") {
    val input =
      """ y = 1000;
          z = function(x = 5, foo = {2 + 4}, fob, ...) { x + fob + foo + y };
          z(,,12) """
    // todo to change when printing is fixed
    assert(evaluate(input).toString == "List(1023)")
  }

  test("argument matching mechanism test") {
    val input =
      """ x = function(e= 5, roo = 6, ros=100, ..., fy = 8) { e + roo + ros + fy};
          x(1, ro=12, ros=1000, q, w, e, r, y,t, fy=1) """
    // todo to change when printing is fixed
    assert(evaluate(input).toString == "List(1014)")
  }
}