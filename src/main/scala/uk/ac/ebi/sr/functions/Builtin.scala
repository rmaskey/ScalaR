package uk.ac.ebi.sr
package functions

import interpreter._
import model.RVal.RChar
import model._

/**
 * Abstract class for functions. Does the arg matching.
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
abstract class Builtin extends RObject with ArgMatching with RFunction {

  //todo should it be global environment. do we actually need this default one?
  lazy val defaultEvaluator = new Evaluator(Environment.emptyEnv)

  lazy val `type` = Type.BUILTIN

  def apply(args: List[FCallArg], fEnv: Environment) = {
    val newEnv = evalArgs(args, fEnv, fEnv)
    val res = process(newEnv)
    if (!newEnv.isBound) newEnv.cleanAll
    res
  }

  /**
   * abstract method that actually executes the function
   *
   * @param env environment with the matched arguments
   */
  protected def process(env: Environment): RObject
}

/**
 * function that returns type of the RObject
 */
case object TypeOf extends StdBuiltin {

  def apply(r: RObject) = RChar(r.`type`.toString)
}
