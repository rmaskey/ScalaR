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
import interpreter.{DeclArg, FDeclArg, NULL}

/**
 * Simplifies the function object realisation since there many
 * functions in R that have only one parameter (usually x)
 *
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */

abstract class StdBuiltin(val X: String) extends Builtin {

  def this() = this(StdBuiltin.X)

  val params = List[FDeclArg](DeclArg(X))

  protected def process(e: Environment) = e resolve X match {
    case Some(x) => apply(x)
    case None => NULL
  }

  protected def apply(r: RObject): RObject
}

object StdBuiltin {
  val X = "x"
}
