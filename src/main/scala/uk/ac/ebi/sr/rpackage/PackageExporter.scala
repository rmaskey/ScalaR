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
package uk.ac.ebi.sr.rpackage

import collection.JavaConversions._
import uk.ac.ebi.sr.model.RObject

/**
 * A trait used for loading packages by jar files.
 * Packages should contain a class mixing this trait and it shoulb written as Main-class
 * in Manifest 
 *
 * Date: Aug 13, 2010
 * @author Taalai Djumabaev
 */
trait PackageExporter {

  /**
   * Method that is used for export
   */
  def exportS: collection.mutable.Map[String, RObject] = exportJ

  /**
   * Method for implementation from Java language. The resulting collection will be converted
   * to scala.collection.mutable.Map and returned by exportS method
   */
  def exportJ: java.util.Map[String, RObject] = error("unimplemented method for export")
}

object PackageExporter