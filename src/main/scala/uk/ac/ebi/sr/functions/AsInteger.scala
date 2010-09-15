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

import interpreter.{DeclArg, FDeclArg, NULL}
import model.RVal._
import model.{RList, Environment, Complex, RObject}
import Operations.{convertArray, concatArrays}

/**
 * Coercion functions
 *
 * Date: Aug 2, 2010
 * @author Taalai Djumabaev
 */

case object AsLogical extends StdBuiltin with ResultInBool {
  import rutils.BoolCoercion._

  // needed to pass it as a paramter. Normally use the AsLogical(x) variant.
  def `as.logical`(r: RObject): RBool = apply(r)

 //TODO coercions should be made properly. No attribute copying is done for now
  def apply(r: RObject): RBool = r match {
    case b: RBool => b
    case i: RInt => applyForBool(i, int2Bool)
    case d: RDouble => applyForBool(d, (e: Double) => double2Bool(e))
    case c: RComplex => applyForBool(c, (e: Complex) => complex2Bool(e))
    case c: RChar => applyForBool(c, (e: String) => char2Bool(e))//in if - error("argument is not interpretable as logical")))
    case l: RList => (concatArrays(l.s.toSeq, apply)(RBool.m)).asInstanceOf[RBool]
    case _ => error("argument of type " + r.`type` + " is not interpretable as logical vector")
  }
}

case object AsInteger extends StdBuiltin with ResultInInt {
  import rutils.IntCoercion._

  // needed to pass it as a paramter. Normally use the AsInteger(x) variant.
  def `as.integer`(r: RObject) = apply(r)

  def apply(r: RObject): RInt = r match {
    case b: RBool => RInt(b.s) // unless we change the bool type
    case i: RInt => i
    case d: RDouble => applyForInt(d, (e: Double) => double2Int(e))
    case c: RComplex => applyForInt(c, (e: Complex) => complex2Int(e))
    case c: RChar => applyForInt(c, (e: String) => char2Int(e))
    case l: RList => (concatArrays(l.s.toSeq, apply)(RInt.m)).asInstanceOf[RInt]
    case _ => error("argument of type " + r.`type` + " is not interpretable as integer vector")
  }
}

case object AsDouble extends StdBuiltin with ResultInDouble {
  import rutils.DoubleCoercion._

  // needed to pass it as a paramter. Normally use the AsDouble(x) variant.
  def `as.double`(r: RObject) = apply(r)

  def apply(r: RObject): RDouble = r match {
    case b: RBool => applyForDouble(b, (e: Int) => e.toDouble)
    case i: RInt => applyForDouble(i, (e: Int) => e.toDouble)
    case d: RDouble => d
    case c: RComplex => applyForDouble(c, (e: Complex) => complex2Double(e))
    case c: RChar => applyForDouble(c, (e: String) => char2Double(e))
    case l: RList => (concatArrays(l.s.toSeq, apply)(RDouble.m)).asInstanceOf[RDouble]
    case _ => error("argument of type " + r.`type` + " is not interpretable as double vector")
  }
}

case object AsCharacter extends StdBuiltin with ResultInChar {
  import rutils.CharCoercion._

  // needed to pass it as a paramter. Normally use the AsCharacter(x) variant.
  def `as.character`(r: RObject) = apply(r)

  def apply(r: RObject): RChar = r match {
    case b: RBool => applyForChar(b, (e: Int) => int2Char(e))
    case i: RInt => applyForChar(i, (e: Int) => int2Char(e))
    case d: RDouble => applyForChar(d, (e: Double) => double2Char(e))
    case c: RComplex => applyForChar(c, (e: Complex) => complex2Char(e))
    case c: RChar => c
    case l: RList => (concatArrays(l.s.toSeq, apply)(RChar.m)).asInstanceOf[RChar]
    case _ => error("argument of type " + r.`type` + " is not interpretable as double vector")
  }
}