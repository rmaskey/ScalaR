package uk.ac.ebi.sr
package functions

import interpreter._
import model._

/**
 *
 * Date: Jun 28, 2010
 * @author Taalai Djumabaev
 */
trait RFunction[T] extends ((List[T], Environment) => Any)

case class Closure(val params: List[FDeclArg], expr: Expression, env: Environment)
        extends RObject with ArgMatching with RFunction[FCallArg] {
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

case object Println extends RFunction[Any] {
  import collection.mutable.LinkedHashSet
  val printLines = LinkedHashSet[String]()

  def formals = null

  def body = null

  def environment = null

  def getAndClearLines = {
    val tmp = printLines.mkString("\n")
    printLines.clear
  }

  def apply(args: List[Any], e: Environment) = args match {
    case (v: String) :: Nil => {println("\"" + v + "\""); v}
    case v :: Nil => {println(v); printLines += v.toString; v}
    case _ => error("println needs 1 argument")
  }
}























