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
package rutils

import uk.ac.ebi.sr.model.Complex

/**
 * NA values for primitive types
 * 
 * Date: Jul 13, 2010
 * @author Taalai Djumabaev
 */
object NAs {
  val boolNA: Int = java.lang.Integer.MAX_VALUE
  val intNA: Int = java.lang.Integer.MAX_VALUE
  val doubleNA: Double = java.lang.Double.MAX_VALUE
  val complexNA: Complex = null.asInstanceOf[Complex]
  val charNA: String = null.asInstanceOf[String]
}