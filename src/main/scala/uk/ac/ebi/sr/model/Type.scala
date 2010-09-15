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
package uk.ac.ebi.sr.model

/**
 * type of R objects
 */
object Type extends Enumeration {  
  type Type = Value

  val NULL = Value("NULL")
  val SYMBOL = Value("symbol")
  val PAIRLIST = Value("pairlist")
  val CLOSURE = Value("closure")
  val ENVIRONMENT = Value("environment")
  val PROMISE = Value("promise")
  val LANGUAGE = Value("language")
  val SPECIAL = Value("special")
  val BUILTIN = Value("builtin")
  val CHAR = Value("char")
  val LOGICAL = Value("logical")
  val INTEGER = Value("integer")
  val DOUBLE = Value("double")
  val COMPLEX = Value("complex")
  val CHARACTER = Value("character")
  val DOTDOTDOT = Value("dotdotdot")
  val ANY = Value("any")
  val EXPRESSION = Value("expression")
  val LIST = Value("list")
  val BYTECODE = Value("bytecode")
  val EXTERNALPTR = Value("externalptr")
  val WEAKREF = Value("weakref")
  val RAW = Value("raw")
  val S4 = Value("s4")

  val TYPE_VALUE =
    Map(LOGICAL -> 1,
      INTEGER -> 2,
      DOUBLE -> 3,
      COMPLEX -> 4,
      CHARACTER -> 5)

  /**
   * method used in concatenation to find the most abstract type
   */
  def typeValue(t: Value) = {
    TYPE_VALUE.get(t) match {
      case Some(x) => x
      case None => 6
    }
  }
}