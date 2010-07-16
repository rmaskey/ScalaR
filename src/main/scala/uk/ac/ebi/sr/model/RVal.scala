package uk.ac.ebi.sr
package model

import collection.mutable.ArrayBuffer
import reflect.Manifest
import interpreter.NULL
import rutils.NAs

/**
 *
 * Date: Jun 25, 2010
 * @author Taalai Djumabaev
 */

trait Sequential[S] extends RObject {
  def s: Array[S]
  lazy val length = if (s == null) 0 else s.length

  def replicate(len: Int): Array[S] = s

  def applyF(f: => Array[S]): Sequential[S]

  def applyChange(f: Array[S] => Any): Sequential[S] = if (isNotReferenced) {
    f(this.s)
    this
  } else {
    val cloned = this.clone.asInstanceOf[Sequential[S]]
    f(cloned.s)
    cloned
  }

  def isNa(t: S): Boolean = t == NA
  def isEmpty = s == null || s.length == 0
  val NA: S
  val m: Manifest[S]
}

trait RVal[T] extends Sequential[T] {

  def empty: RVal[T]

  override def toString = if (isEmpty) NULL.asString else s.toList.toString
}



object RVal {
  abstract class RList extends RVal[Any] {

  }

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