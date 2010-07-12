package uk.ac.ebi.sr
package functions

import interpreter._
import model._

/**
 *
 * Date: Jun 28, 2010
 * @author Taalai Djumabaev
 */
trait RFunction extends ((List[FCallArg], Environment) => RObject)

case class Closure(val params: List[FDeclArg], expr: Expression, env: Environment)
        extends RObject with ArgMatching with RFunction {
  lazy val defaultEvaluator = new Evaluator(env)
  lazy val `type` = Type.CLOSURE
  import scala.collection.mutable.Map

  def formals = params

  def body = expr                         

  def environment = env

  def apply(args: List[FCallArg], fEnv: Environment) = {
    Evaluator.eval(expr, evalArgs(args, fEnv, env))
  }

  override def toString() = "Closure"
}























