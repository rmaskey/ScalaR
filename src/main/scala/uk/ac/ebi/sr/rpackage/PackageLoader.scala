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
import model.{RObject, Environment}
import model.RVal.RChar

import interpreter.{DeclArg, FDeclArg, NULL}
import java.io.{FileFilter, File}

/**
 * An object that loads a package. Looks for packages in session's 'libs' paths
 * First found package is loaded
 *
 * Date: Aug 16, 2010
 * @author Taalai Djumabaev
 */

object PackageLoader extends Builtin {

  val PACKAGE = "package"
  val params = List[FDeclArg](DeclArg(PACKAGE))

  protected def process(env: Environment): RObject = env.resolve(PACKAGE) match {
    case Some(r: RChar) => if (r.length == 1) loadPackage(r.s(0)) else NULL
    case _ => NULL
  }

  def loadPackage(r: String): RObject = {
    val session = RSession.currentSession
    val loaded = session.loadedLibraries
    if (!loaded.contains(r)) {
      RSession.libs.foreach((s: String) => {
        val files = (new File(s)).listFiles(new FileFilter {
          def accept(f: File) = f.isDirectory
        })
        files.foreach((f: File) =>
          if (f.getName == r) { RPackage.loadPackage(f, session.base, session); return NULL })
      })
    }
    //todo warning here: package 'r' not found
    NULL //todo print will be RObject later (print(null) - should print nothing)
  }
}