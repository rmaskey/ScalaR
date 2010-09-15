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
