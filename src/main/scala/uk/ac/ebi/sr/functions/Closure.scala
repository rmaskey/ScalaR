/*
 * Copyright (c) 2009-2010 European Molecular Biology Laboratory
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ebi.sr
package functions

import interpreter._
import model._

/**
 * Trait that is used by interpreter for calling functions.
 * Every function should be mixed with this trait.
 *
 * Date: Jun 28, 2010
 * @author Taalai Djumabaev
 */
trait RFunction extends ((List[FCallArg], Environment) => RObject)


/**
 * Closure as in R language. Changing of body or environment is not yet supported.
 */
case class Closure(val params: List[FDeclArg], expr: Expression, env: Environment)
        extends RObject with ArgMatching with RFunction {
  lazy val defaultEvaluator = new Evaluator(env)
  lazy val `type` = Type.CLOSURE
  import scala.collection.mutable.Map

  def formals = params

  def body = expr                         

  def environment = env

  def apply(args: List[FCallArg], fEnv: Environment) = {
    val newEnv = evalArgs(args, fEnv, env)
    val res = Evaluator.eval(expr, newEnv)
    if (!newEnv.isBound) newEnv.cleanAll
    res
  }

  override def toString() = "Closure"
}























