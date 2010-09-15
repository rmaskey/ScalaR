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

import model.{Environment, RObject}
import interpreter._

/**
 * Trait for functions that get/set values.
 * ex: length(x) <- 3
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
trait Assignable extends Builtin with ArgMatching {

  /**
   * Sets the new value for the function. It depends on the args which value will be changed
   *
   * @param args arguments of the function returned from parser. List is never empty since
   * when no argument supplied List(NoneArg) is returned
   * @param fEnv environment of the function
   * @param newValue value to be set
   */
  def `<-`(args: List[FCallArg], fEnv: Environment, newValue: RObject): RObject = {
    //it is never empty list
    val name = args.head match {
      case CallArg(Var(i)) => i
      case CallArg(Lit(i)) => i
      case CallArgDef(n, Var(i)) => i
      case CallArgDef(n, Lit(i)) => i
      case NoneArg => error("invalid (NULL) left side of assignment")
      case _ => error("target of assignment expands to non-language object")
    }
    val newEnv = evalArgs(args, fEnv, fEnv)
    val res = assign(newEnv, newValue)
    fEnv.resolveWithEnv(name) match {
      case Some((o, e)) => if (o != res) {
        o.removeReferencer
        e += (name, res)
      }
      case None => //can't get here
    }
    if (!newEnv.isBound) newEnv.cleanAll
    res
  }

  /**
   * method where assignment is done. `<-` method is used by interpreter to match the arguments
   *
   * @param env environment with matched arguments to the params (inherited from Builtin)
   * @param newValue to be set. which value will be replaced depends on arguments matched
   */
  protected def assign(env: Environment, newValue: RObject): RObject
}