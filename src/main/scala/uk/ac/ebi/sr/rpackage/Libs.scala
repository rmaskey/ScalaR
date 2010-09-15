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
package rpackage

import functions.Builtin
import interpreter.{NULL, DeclArg, FDeclArg}
import model.{RObject, Environment}
import model.RVal.RChar

/**
 * Function for adding new directories with r-packages in them
 *
 * Date: Sep 7, 2010
 * @author Taalai Djumabaev
 */

object Libs extends Builtin {

  val DIR = "dir"
  val params = List[FDeclArg](DeclArg(DIR))

  protected def process(env: Environment): RObject = env.resolve(DIR) match {
    case Some(s: RChar) => {
      if (s.length == 1) addLibsDir(s.s(0))
      //todo warnings otherwise
      NULL
    }
    case _ => NULL
  }

  def addLibsDir(s: String) = RSession.libs += s
}