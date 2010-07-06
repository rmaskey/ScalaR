package uk.ac.ebi.sr.model

import collection.mutable.ArrayBuffer
import reflect.Manifest

/**
 *
 * Date: Jun 25, 2010
 * @author Taalai Djumabaev
 */

trait Sequential[S] {
  def s: Array[S]

  def replicate(len: Int): Array[S] = s

  def resize(i: Int): Sequential[S]
  val NA: S
}

trait RVal[T] extends RObject with Sequential[T] {

  def isNa(t: T): Boolean = t == NA

  def isEmpty = s == null || s.length == 0

  override lazy val length = if (s == null) 0 else s.length
  override def toString = if (isEmpty) "NULL" else s.toList.toString
}



object RVal {
  abstract class RList extends RVal[Any] {

  }

  type Bool = Int
  implicit def rbool2RInt(b: RBool) = new RInt(b.s)
  class RBool(val s: Array[Bool]) extends RVal[Bool] {
    lazy val `type` = RBool.`type`
    lazy val NA = RBool.NA

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
      override def toString = "NULL"
    }
    val NA = java.lang.Integer.MAX_VALUE
    val `type` = Type.LOGICAL
  }

  class RInt(val s: Array[Int]) extends RVal[Int] {
    lazy val `type` = RInt.`type`
    lazy val NA =RInt.NA

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
      override def toString = "NULL"
    }
    val `type` = Type.INTEGER
    val NA = java.lang.Integer.MAX_VALUE
  }

  class RDouble(val s: Array[Double]) extends RVal[Double] {
    lazy val `type` = RDouble.`type`
    lazy val NA = RDouble.NA

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
      override def toString = "NULL"
    }
    val `type` = Type.DOUBLE
    val NA = java.lang.Double.MAX_VALUE
  }

  class RComplex(val s: Array[Complex]) extends RVal[Complex] {
    lazy val `type` = RComplex.`type`
    lazy val NA = RComplex.NA

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
      override def toString = "NULL"
    }
    val `type` = Type.COMPLEX
    val NA = null.asInstanceOf[Complex]
  }

  class RChar(val s: Array[String]) extends RVal[String] {
    lazy val `type` = RChar.`type`
    lazy val NA =RChar.NA

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
      override def toString = "NULL"
    }
    val `type` = Type.CHARACTER // or char?
    val NA = null.asInstanceOf[String]
  }
}

object Operations {
  import Complex._
  import RVal._
  def boolean2Int(b: Boolean) = b match {
    case true => 1
    case false => 0
  }

  def modeIterate[A, B, C](l: RVal[A], r: RVal[B], f: (A, B) => C, NA: C)(implicit m: Manifest[C]): Array[C] = {
    if (l.isEmpty || r.isEmpty) return null
    val la = l.s
    val ra = r.s
    val count = if (la.length > ra.length) la.length else ra.length
    val buf = new ArrayBuffer[C](count)

    var i, j, k = 0;
    while(k < count) {
      buf += (if (la(i) == l.NA || ra(j) == r.NA) NA else f(la(i), ra(j)))
      i = if (i + 1 < la.length) i + 1 else 0
      j = if (j + 1 < ra.length) j + 1 else 0
      k += 1
    }
    buf.toArray
  }

  def genSequence[B](l: RInt, r: RVal[B])(implicit f: B => Int): Array[Int] = {
    if (l.isEmpty || r.isEmpty) return null
    // warning for length > 1
    val start = l.s(0)
    val end = f(r.s(0))
    val cond = if (start < end) (i: Int) => i <= end else (i: Int) => i >= end
    val buf = new ArrayBuffer[Int]
    var i = start
    while(cond(i)) {
      buf += i
      i += (if (start < end) 1 else -1)
    }
    buf.toArray
  }

  def genSequence[B](l: RDouble, r: RVal[B])(implicit f: B => Double): Array[Double] = {
    if (l.isEmpty || r.isEmpty) return null
    // warning for length > 1
    val start = l.s(0)
    val end = f(r.s(0))
    val cond = if (start < end) (i: Double) => i <= end else (i: Double) => i >= end
    val buf = new ArrayBuffer[Double]
    var i = start
    while(cond(i)) {
      buf += i
      i += (if (start < end) 1 else -1)
    }
    buf.toArray
  }

  def genSequence[B](l: RComplex, r: RVal[B])(implicit f: B => Double): Array[Double] = {
    if (l.isEmpty || r.isEmpty) return null
    // warning for length > 1
    val start = l.s(0).r
    val end = f(r.s(0))
    val cond = if (start < end) (i: Double) => i <= end else (i: Double) => i >= end
    val buf = new ArrayBuffer[Double]
    var i = start
    while(cond(i)) {
      buf += i
      i += (if (start < end) 1 else -1)
    }
    buf.toArray
  }

  def sum(l: RInt, r: RInt): RInt = RInt(modeIterate(l, r, (f: Int, s: Int) => f + s, RInt.NA))
  def sum(l: RInt, r: RDouble): RDouble = RDouble(modeIterate(l, r, (f: Int, s: Double) => f.toDouble + s, RDouble.NA))
  def sum(l: RInt, r: RComplex): RComplex = RComplex(modeIterate(l, r, (f: Int, s: Complex) => int2Complex(f) + s, RComplex.NA))

  def sum(l: RDouble, r: RInt): RDouble = RDouble(modeIterate(l, r, (f: Double, s: Int) => f + s.toDouble, RDouble.NA))
  def sum(l: RDouble, r: RDouble): RDouble = RDouble(modeIterate(l, r, (f: Double, s: Double) => f + s, RDouble.NA))
  def sum(l: RDouble, r: RComplex): RComplex = RComplex(modeIterate(l, r, (f: Double, s: Complex) => double2Complex(f) + s, RComplex.NA))

  def sum(l: RComplex, r: RInt): RComplex = RComplex(modeIterate(l, r, (f: Complex, s: Int) => f + int2Complex(s), RComplex.NA))
  def sum(l: RComplex, r: RDouble): RComplex = RComplex(modeIterate(l, r, (f: Complex, s: Double) => f + double2Complex(s), RComplex.NA))
  def sum(l: RComplex, r: RComplex): RComplex = RComplex(modeIterate(l, r, (f: Complex, s: Complex) => f + s, RComplex.NA))
                                                               

  def lt(l: RInt, r: RInt): RBool = RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f < s), RBool.NA))
  def lt(l: RInt, r: RDouble): RBool = RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f.toDouble < s), RBool.NA))
  def lt(l: RInt, r: RChar): RBool = RBool(modeIterate(l, r, (f: Int, s: String) => boolean2Int(f.toString < s), RBool.NA))

  def lt(l: RDouble, r: RInt): RBool = RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f.toDouble < s), RBool.NA))
  def lt(l: RDouble, r: RDouble): RBool = RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f < s), RBool.NA))
  def lt(l: RDouble, r: RChar): RBool = RBool(modeIterate(l, r, (f: Double, s: String) => boolean2Int(f.toString < s), RBool.NA))

  def lt(l: RChar, r: RInt): RBool = RBool(modeIterate(l, r, (f: String, s: Int) => boolean2Int(f < s.toString), RBool.NA))
  def lt(l: RChar, r: RDouble): RBool = RBool(modeIterate(l, r, (f: String, s: Double) => boolean2Int(f < s.toString), RBool.NA))
  def lt(l: RChar, r: RChar): RBool = RBool(modeIterate(l, r, (f: String, s: String) => boolean2Int(f < s), RBool.NA))


  def seq(l: RInt, r: RInt): RInt = RInt(genSequence(l, r))
  def seq(l: RInt, r: RDouble): RInt = RInt(genSequence(l, r)((d: Double) => d.toInt))
  def seq(l: RInt, r: RComplex): RInt = RInt(genSequence(l, r)((c: Complex) => c.r.toInt))

  def seq(l: RDouble, r: RInt): RDouble = RDouble(genSequence(l, r))
  def seq(l: RDouble, r: RDouble): RDouble = RDouble(genSequence(l, r))
  def seq(l: RDouble, r: RComplex): RDouble = RDouble(genSequence(l, r)((c: Complex) => c.r))

  def seq(l: RComplex, r: RInt): RDouble = RDouble(genSequence(l, r))
  def seq(l: RComplex, r: RDouble): RDouble = RDouble(genSequence(l, r))
  def seq(l: RComplex, r: RComplex): RDouble = RDouble(genSequence(l, r)((c: Complex) => c.r))




  def main(args: Array[String]) {
    println(sum(RInt(Array.tabulate(1000)(_.toInt)), RDouble(Array.tabulate(10000)(_.toDouble))))
    println(sum(RInt(1,2,3,4, 6, 7,2), RDouble(1.4,2.3, 5)))
    println(sum(RInt(1,2,3), RDouble(1.4,2.3, 5, 4, 3,56 ,6)))
    println(sum(RBool(1,2,3,4, 6, 7,2), RComplex(4.4,2.3, 5)))
    println(sum(RBool(1,2,32), RBool()))
    //new Array[Double](10000000)
    //println(seq(RInt))
  }
}

