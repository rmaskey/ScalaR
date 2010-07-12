package uk.ac.ebi.sr.model

import collection.mutable.ArrayBuffer
import reflect.Manifest
import uk.ac.ebi.sr.interpreter.NULL

/**
 *
 * Date: Jun 25, 2010
 * @author Taalai Djumabaev
 */

trait Sequential[S] extends RObject {
  def s: Array[S]
  lazy val length = if (s == null) 0 else s.length

  def replicate(len: Int): Array[S] = s

  def resize(i: Int): Sequential[S]
  val NA: S
}

trait RVal[T] extends Sequential[T] {

  def isNa(t: T): Boolean = t == NA

  def isEmpty = s == null || s.length == 0
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
    def empty = RBool()

    def resize(i: Int): RBool = {
      if (i == length) return this
      val newSeq = Array.tabulate(i)(_ => NA)
      s.copyToArray(newSeq, 0, math.min(length, i))
      //all irrelevant attributes are not copied
      new RBool(newSeq)
    }
  }
  object RBool {
    def apply(xs: Bool*) = new RBool(xs.toArray)
    def apply(s: Array[Bool]) = if (s == null) BoolNull else new RBool(s)
    //not needed. since Int is used
    def apply() = BoolNull
    object BoolNull extends RBool(null) {
      override def toString = NULL.asString
    }
    val NA = java.lang.Integer.MAX_VALUE
    val `type` = Type.LOGICAL
  }

  class RInt(val s: Array[Int]) extends RVal[Int] {
    lazy val `type` = RInt.`type`
    lazy val NA =RInt.NA
    def empty = RInt()

    def resize(i: Int): RInt = {
      if (i == length) return this
      val newSeq = Array.tabulate(i)(_ => NA)
      s.copyToArray(newSeq, 0, math.min(length, i))
      //all irrelevant attributes are not copied
      new RInt(newSeq)
    }
  }
  object RInt {
    def apply(xs: Int*) = new RInt(xs.toArray)
    def apply(s: Array[Int]) = if (s == null) IntNull else new RInt(s)
    def apply() = IntNull
    object IntNull extends RInt(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.INTEGER
    val NA = java.lang.Integer.MAX_VALUE
  }

  class RDouble(val s: Array[Double]) extends RVal[Double] {
    lazy val `type` = RDouble.`type`
    lazy val NA = RDouble.NA
    def empty = RDouble()

    def resize(i: Int): RDouble = {
      if (i == length) return this
      val newSeq = Array.tabulate(i)(_ => NA)
      s.copyToArray(newSeq, 0, math.min(length, i))
      //all irrelevant attributes are not copied
      new RDouble(newSeq)
    }
  }
  object RDouble {
    def apply(xs: Double*) = new RDouble(xs.toArray)
    def apply(s: Array[Double]) = if (s == null) DoubleNull else new RDouble(s)
    def apply() = DoubleNull
    object DoubleNull extends RDouble(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.DOUBLE
    val NA = java.lang.Double.MAX_VALUE
  }

  class RComplex(val s: Array[Complex]) extends RVal[Complex] {
    lazy val `type` = RComplex.`type`
    lazy val NA = RComplex.NA
    def empty = RComplex()

    def resize(i: Int): RComplex = {
      if (i == length) return this
      val newSeq = Array.tabulate(i)(_ => NA)
      s.copyToArray(newSeq, 0, math.min(length, i))
      //all irrelevant attributes are not copied
      new RComplex(newSeq)
    }
  }
  object RComplex {
    def apply(xs: Complex*) = new RComplex(xs.toArray)
    def apply(s: Array[Complex]) = if (s == null) ComplexNull else new RComplex(s)
    def apply() = ComplexNull
    object ComplexNull extends RComplex(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.COMPLEX
    val NA = null.asInstanceOf[Complex]
  }

  class RChar(val s: Array[String]) extends RVal[String] {
    lazy val `type` = RChar.`type`
    lazy val NA =RChar.NA
    def empty = RChar()
    
    def resize(i: Int): RChar = {
      if (i == length) return this
      val newSeq = Array.tabulate(i)(_ => NA)
      s.copyToArray(newSeq, 0, math.min(length, i))
      //all irrelevant attributes are not copied
      new RChar(newSeq)
    }
  }
  object RChar {
    def apply(xs: String*) = new RChar(xs.toArray)
    def apply(s: Array[String]) = if (s == null) CharNull else new RChar(s)
    def apply() = CharNull
    object CharNull extends RChar(null) {
      override def toString = NULL.asString
    }
    val `type` = Type.CHARACTER // or char?
    val NA = null.asInstanceOf[String]
  }
}