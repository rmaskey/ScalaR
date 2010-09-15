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

import collection.mutable.Map
import functions._
import model.Environment
import java.io.{FileFilter, File}
import rpackage.{Libs, PackageLoader, RPackage}

/**
 * Only one session per scalar is supported for now.
 * 
 * Date: Aug 11, 2010
 * @author Taalai Djumabaev
 */
class RSession {

  //todo for now only one session exists
  val global = RSession.globalEnv
  val base = RSession.baseEnv

  val loadedLibraries = Map[String, RPackage]()
  val waitingPacks = {
    val set = collection.mutable.Set[File]()
    RSession.libs.foreach((s: String) =>
      set ++ (new File(s)).listFiles(new FileFilter {
        def accept(f: File) = f.isDirectory
      }))
    set
  }

  waitingPacks.foreach((f: File) =>
    if (!loadedLibraries.contains(f.getName)) RPackage.loadPackage(f, base, this)
    else {}) //todo warning
}

object RSession {
  val packages = "packages/"
  val libs = scala.collection.mutable.ListBuffer[String](packages)

  val baseEnv = Environment.emptyEnv ++= Map(
    "attr" -> Attr,
    "attributes" -> Attributes,
    "length" -> Length,
    "as.integer" -> AsInteger,
    "as.logical" -> AsLogical,
    "as.double" -> AsDouble,
    "as.character" -> AsCharacter,
    "c" -> Concat,
    "typeof" -> TypeOf,
    "library" -> PackageLoader,
    "list" -> RLangList,
    "addLibDir" -> Libs,
    "library" -> PackageLoader,
    "sin" -> Sine)
  val globalEnv = Environment.childEnv(baseEnv)
  
  val currentSession = new RSession //todo to change
}