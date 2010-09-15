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
package model

import collection.mutable.ArrayBuffer
import reflect.Manifest
import interpreter.NULL
import rutils.NAs

/**
 * Vector like objects in R language are implemented by Sequential trait
 *
 * Date: Jun 25, 2010
 * @author Taalai Djumabaev
 */
trait Sequential[S] extends RObject {

  def s: Array[S]
  lazy val length = if (s == null) 0 else s.length

  /**
   * check a condition for every element
   */
  def forall(f: S => Boolean): Boolean = {
    var i = 0
    while (i < length) if (!f(s(i))) return false else i += 1
    true
  }

  def applyF(f: => Array[S]): Sequential[S]      //todo should also copy needed attributes. not all of them! which ones?

  def applyChange(f: Array[S] => Any): Sequential[S] = if (isMultiReferenced) {    //todo write a test. or referenced once?
    val cloned = this.clone.asInstanceOf[Sequential[S]]
    f(cloned.s)
    cloned
  } else {
    f(this.s)
    this
  }

  def isNa(t: S): Boolean = t == NA
  def isEmpty = s == null || s.length == 0
  val NA: S
  val m: Manifest[S]

  override def toString = if (isEmpty) NULL.asString else s.toList.toString
}

/**
 * vectors of primitive types are mixed with this trait.
 */
trait RVal[T] extends Sequential[T] {

  def empty: RVal[T]
}

object RVal {

  type Bool = Int
  implicit def rbool2RInt(b: RBool): RInt = if (b.isEmpty) RInt() else new RInt(b.s)

  class RBool(val s: Array[Bool]) extends RVal[Bool] {
    lazy val `type` = RBool.`type`
    lazy val NA = RBool.NA
    lazy val m = RBool.m
    def empty = RBool()

    def applyF(f: => Array[Bool]) = RBool(f)
  }
  object RBool {
    def apply(xs: Bool*) = new RBool(xs.toArray)
    def apply(s: Array[Bool]) = if (s == null) BoolNull else new RBool(s)
    //not needed. since Int is used
    def apply() = BoolNull
    object BoolNull extends RBool(null) {
      override def toString = NULL.asString
    }
    val NA = NAs.intNA
    val `type` = Type.LOGICAL
    val m = Manifest.Int
  }

  class RInt(val s: Array[Int]) extends RVal[Int] {
    lazy val `type` = RInt.`type`
    lazy val NA = RInt.NA
    lazy val m = RInt.m
    def empty = RInt()

    def applyF(f: => Array[Int]) = RInt(f)

  }
  object RInt {
    def apply(xs: Int*) = new RInt(xs.toArray)
    def apply(s: Array[Int]) = if (s == null) IntNull else new RInt(s)
    def apply() = IntNull
    object IntNull extends RInt(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.INTEGER
    val NA = NAs.intNA
    val m = Manifest.Int
  }

  class RDouble(val s: Array[Double]) extends RVal[Double] {
    lazy val `type` = RDouble.`type`
    lazy val NA = RDouble.NA
    lazy val m = RDouble.m
    def empty = RDouble()

    def applyF(f: => Array[Double]) = RDouble(f)
  }
  object RDouble {
    def apply(xs: Double*) = new RDouble(xs.toArray)
    def apply(s: Array[Double]) = if (s == null) DoubleNull else new RDouble(s)
    def apply() = DoubleNull
    object DoubleNull extends RDouble(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.DOUBLE
    val NA = NAs.doubleNA
    val m = Manifest.Double
  }

  class RComplex(val s: Array[Complex]) extends RVal[Complex] {
    lazy val `type` = RComplex.`type`
    lazy val NA = RComplex.NA
    lazy val m = RComplex.m
    def empty = RComplex()

    def applyF(f: => Array[Complex]) = RComplex(f)
  }
  object RComplex {
    def apply(xs: Complex*) = new RComplex(xs.toArray)
    def apply(s: Array[Complex]) = if (s == null) ComplexNull else new RComplex(s)
    def apply() = ComplexNull
    object ComplexNull extends RComplex(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.COMPLEX
    val NA = NAs.complexNA
    val m = Manifest.classType[Complex](classOf[Complex])
  }

  class RChar(val s: Array[String]) extends RVal[String] {
    lazy val `type` = RChar.`type`
    lazy val NA = RChar.NA
    lazy val m = RChar.m
    def empty = RChar()

    def applyF(f: => Array[String]) = RChar(f)
  }
  object RChar {
    def apply(xs: String*) = new RChar(xs.toArray)
    def apply(s: Array[String]) = if (s == null) CharNull else new RChar(s)
    def apply() = CharNull
    object CharNull extends RChar(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.CHARACTER // or char?
    val NA = NAs.charNA
    val m = Manifest.classType[String](classOf[String])
  }
}