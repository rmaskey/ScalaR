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

  test("argument evaluation, environment binding test") {
    val input =
      """ y = 1
          x = function(z, s = { y = 1000; y + 1}) {s + z}
          x(2)
          y
      """
    assert(evaluate(input).toString == "List(1000)")
  }

  test("environment LEXICAL binding test") {
    val input =
      """ a = 1
          f = function(x) { y <- 10; g <- function(x) x + y; g}; h <- f(1); h(3)
      """
    assert(evaluate(input).toString == "List(13)")
  }

  test("environment binding test") {
    val input =
      """ a = 1
          f = function(x) { g <- function(x) x; g}; h <- f(1); h(3)
      """
    assert(evaluate(input).toString == "List(3)")
  }

  test("argument evaluation, environment binding test 2") {
    val input =
      """ y = 1
          x = function(s = { y = 1000; y + 1}) {s}
          x()
          y
      """
    assert(evaluate(input).toString == "List(1000)")
  }

  test(" no arguments function test ") {
    val input =
    """
        x = 5                 ;
        y = function() 10 + x;
        x = 133+2;
        "y"()
    """
    assert(evaluate(input).toString == "List(145)")
  }

  test(" user defined operation test ") {
    val input =
    """ "%xyz%" = function(a, b) a + b
         1 %xyz% 2 + 2.5

    """
    assert(evaluate(input).toString == "List(5.5)")
  }

  test(" default arguments matching test") {
    val input =
    """ q = function(a = 1, b = 10) if (a < b) 0 else 100;
    """
    val tail1 = "q()"
    assert(evaluate(input + tail1).toString == "List(0)")
    val tail2 = "q(,)"
    assert(evaluate(input + tail2).toString == "List(0)")
    val tail3 = "q(, 1)"
    assert(evaluate(input + tail3).toString == "List(100)")
    val tail4 = "q(200)"
    assert(evaluate(input + tail4).toString == "List(100)")
    val tail5 = "q(200,)"
    assert(evaluate(input + tail5).toString == "List(100)")
    val tail7 = "q(b = 2, a = 3)"
    assert(evaluate(input + tail7).toString == "List(100)")
  }

  test (" default arguments matching bug fix ") {
    val input =
    """ q = function(a = 1, b = 10) if (a < b) 0 else 100
        q(b = 0)
    """
    assert(evaluate(input).toString == "List(100)")
  }
}