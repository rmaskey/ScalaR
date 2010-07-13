package uk.ac.ebi.sr.functions

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import uk.ac.ebi.sr.interpreter.{RParser, Expression, Interpreter}

/**
 *
 * Date: Jul 7, 2010
 * @author Taalai Djumabaev
 */

@RunWith(classOf[JUnitRunner])
class OperationsSuite extends FunSuite {

  def evaluate(input: String): Any = {
    RParser.parseUnwrap(input) match {
      case e: Expression => Interpreter.interpret(e)._1
      case _ => None
    }
  }

//  test("argument matching mechanism with environment bindings test ") {
//    val input =
//      """ y = 1000;
//          z = function(x = 5, foo = {2 + 4}, fob, ...) { x + fob + foo + y };
//          z(,,12) """
//    // todo to change when printing is fixed
//    assert(evaluate(input).toString == "List(1023)")
//  }
}
