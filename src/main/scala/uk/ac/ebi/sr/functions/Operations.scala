package uk.ac.ebi.sr
package functions

import collection.mutable.ArrayBuffer
import interpreter.NULL
import model.{Complex, RVal, RObject, Sequential}

/**
 *
 * Date: Jul 7, 2010
 * @author Taalai Djumabaev
 */
object Operations {
  import model.Complex._
  import model.RVal._

  def boolean2Int(b: Boolean) = b match {
    case true => 1
    case false => 0
  }

  import functions.Attr._
  def dim[A, B, F <: RObject](a: Sequential[A], b: Sequential[B], f: => F) = {
    val attribute = (attr(a, DIM), attr(b, DIM)) match {
      //when the dim is set - it is coerced to RInt
      case (l: RInt, r: RInt) => if (true) l else error("non-comformable arrays")
      case (l: RInt, NULL) => if (a.length >= b.length) l else error("non-comformable arrays") //todo also warning if not a multiple
      case (NULL, r: RInt) => if (b.length >= a.length) r else error("non-comformable arrays") //todo also warning if not a multiple
      case (NULL, NULL) => NULL
      case _ => error("non-comformable arrays")
    }
    val res = f
    if (attribute != NULL) `attr<-`(res, DIM, attribute)
    res
  }

  def concatArrays[B](obs: Seq[RObject], f: RObject => Sequential[B])(implicit m: Manifest[B]): RObject = {
    var totalLength = 0
    val list = for (o <- obs) yield {
      val seq = f(o)
      totalLength += seq.length
      seq
    }
    list.head.applyF({
      val res = new Array[B](totalLength)
      var i = 0
      for (l <- list) {
        l.s.copyToArray(res, i)
        i += l.length
      }
      res
    })
  }
  
  def convertArray[A, B](a: Array[A], f: A => B)(implicit m: Manifest[B]): Array[B] = {
    if (a == null || a.length == 0) return null
    val buf = new ArrayBuffer[B](a.length)

    var i = 0
    while (i < a.length) { buf += f(a(i)); i += 1 }
    buf.toArray
  }

  def modeIterate[A, B, C](l: Sequential[A], r: Sequential[B], f: (A, B) => C, NA: C)(implicit m: Manifest[C]): Array[C] = {
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

  def genSequence[B](l: RInt, r: Sequential[B])(implicit f: B => Int): Array[Int] = {
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

  def genSequence[B](l: RDouble, r: Sequential[B])(implicit f: B => Double): Array[Double] = {
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

  def genSequence[B](l: RComplex, r: Sequential[B])(implicit f: B => Double): Array[Double] = {
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

  def sum(l: RInt, r: RInt): RInt = dim(l, r, RInt(modeIterate(l, r, (f: Int, s: Int) => f + s, RInt.NA)))
  def sum(l: RInt, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Int, s: Double) => f.toDouble + s, RDouble.NA)))
  def sum(l: RInt, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Int, s: Complex) => int2Complex(f) + s, RComplex.NA)))

  def sum(l: RDouble, r: RInt): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Int) => f + s.toDouble, RDouble.NA)))
  def sum(l: RDouble, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Double) => f + s, RDouble.NA)))
  def sum(l: RDouble, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Double, s: Complex) => double2Complex(f) + s, RComplex.NA)))

  def sum(l: RComplex, r: RInt): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Int) => f + int2Complex(s), RComplex.NA)))
  def sum(l: RComplex, r: RDouble): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Double) => f + double2Complex(s), RComplex.NA)))
  def sum(l: RComplex, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Complex) => f + s, RComplex.NA)))

  def subtract(l: RInt, r: RInt): RInt = dim(l, r, RInt(modeIterate(l, r, (f: Int, s: Int) => f - s, RInt.NA)))
  def subtract(l: RInt, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Int, s: Double) => f.toDouble - s, RDouble.NA)))
  def subtract(l: RInt, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Int, s: Complex) => int2Complex(f) - s, RComplex.NA)))

  def subtract(l: RDouble, r: RInt): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Int) => f - s.toDouble, RDouble.NA)))
  def subtract(l: RDouble, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Double) => f - s, RDouble.NA)))
  def subtract(l: RDouble, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Double, s: Complex) => double2Complex(f) - s, RComplex.NA)))

  def subtract(l: RComplex, r: RInt): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Int) => f - int2Complex(s), RComplex.NA)))
  def subtract(l: RComplex, r: RDouble): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Double) => f - double2Complex(s), RComplex.NA)))
  def subtract(l: RComplex, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Complex) => f - s, RComplex.NA)))

  def multiply(l: RInt, r: RInt): RInt = dim(l, r, RInt(modeIterate(l, r, (f: Int, s: Int) => f * s, RInt.NA)))
  def multiply(l: RInt, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Int, s: Double) => f.toDouble * s, RDouble.NA)))
  def multiply(l: RInt, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Int, s: Complex) => int2Complex(f) * s, RComplex.NA)))

  def multiply(l: RDouble, r: RInt): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Int) => f * s.toDouble, RDouble.NA)))
  def multiply(l: RDouble, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Double) => f * s, RDouble.NA)))
  def multiply(l: RDouble, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Double, s: Complex) => double2Complex(f) * s, RComplex.NA)))

  def multiply(l: RComplex, r: RInt): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Int) => f * int2Complex(s), RComplex.NA)))
  def multiply(l: RComplex, r: RDouble): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Double) => f * double2Complex(s), RComplex.NA)))
  def multiply(l: RComplex, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Complex) => f * s, RComplex.NA)))

  def divide(l: RInt, r: RInt): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Int, s: Int) => (f / s.toDouble), RDouble.NA)))
  def divide(l: RInt, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Int, s: Double) => f.toDouble / s, RDouble.NA)))
  def divide(l: RInt, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Int, s: Complex) => int2Complex(f) / s, RComplex.NA)))

  def divide(l: RDouble, r: RInt): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Int) => f / s.toDouble, RDouble.NA)))
  def divide(l: RDouble, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Double) => f / s, RDouble.NA)))
  def divide(l: RDouble, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Double, s: Complex) => double2Complex(f) / s, RComplex.NA)))

  def divide(l: RComplex, r: RInt): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Int) => f / int2Complex(s), RComplex.NA)))
  def divide(l: RComplex, r: RDouble): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Double) => f / double2Complex(s), RComplex.NA)))
  def divide(l: RComplex, r: RComplex): RComplex = dim(l, r, RComplex(modeIterate(l, r, (f: Complex, s: Complex) => f / s, RComplex.NA)))

  def pow(l: RInt, r: RInt): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Int, s: Int) => java.lang.Math.pow(f, s), RDouble.NA)))
  def pow(l: RInt, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Int, s: Double) => java.lang.Math.pow(f, s), RDouble.NA)))
  def pow(l: RInt, r: RComplex): RComplex = error("unimplemented operation with complex numbers")

  def pow(l: RDouble, r: RInt): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Int) => java.lang.Math.pow(f, s), RDouble.NA)))
  def pow(l: RDouble, r: RDouble): RDouble = dim(l, r, RDouble(modeIterate(l, r, (f: Double, s: Double) => java.lang.Math.pow(f, s), RDouble.NA)))
  def pow(l: RDouble, r: RComplex): RComplex = error("unimplemented operation with complex numbers")

  def pow(l: RComplex, r: RInt): RComplex = error("unimplemented operation with complex numbers")
  def pow(l: RComplex, r: RDouble): RComplex = error("unimplemented operation with complex numbers")
  def pow(l: RComplex, r: RComplex): RComplex = error("unimplemented operation with complex numbers")


  def lt(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f < s), RBool.NA)))
  def lt(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f.toDouble < s), RBool.NA)))
  def lt(l: RInt, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: String) => boolean2Int(f.toString < s), RBool.NA)))

  def lt(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f.toDouble < s), RBool.NA)))
  def lt(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f < s), RBool.NA)))
  def lt(l: RDouble, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: String) => boolean2Int(f.toString < s), RBool.NA)))

  def lt(l: RChar, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Int) => boolean2Int(f < s.toString), RBool.NA)))
  def lt(l: RChar, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Double) => boolean2Int(f < s.toString), RBool.NA)))
  def lt(l: RChar, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: String) => boolean2Int(f < s), RBool.NA)))

  def lteq(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f <= s), RBool.NA)))
  def lteq(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f.toDouble <= s), RBool.NA)))
  def lteq(l: RInt, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: String) => boolean2Int(f.toString <= s), RBool.NA)))

  def lteq(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f.toDouble <= s), RBool.NA)))
  def lteq(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f <= s), RBool.NA)))
  def lteq(l: RDouble, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: String) => boolean2Int(f.toString <= s), RBool.NA)))

  def lteq(l: RChar, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Int) => boolean2Int(f <= s.toString), RBool.NA)))
  def lteq(l: RChar, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Double) => boolean2Int(f <= s.toString), RBool.NA)))
  def lteq(l: RChar, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: String) => boolean2Int(f <= s), RBool.NA)))

  def gt(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f > s), RBool.NA)))
  def gt(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f.toDouble > s), RBool.NA)))
  def gt(l: RInt, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: String) => boolean2Int(f.toString > s), RBool.NA)))

  def gt(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f.toDouble > s), RBool.NA)))
  def gt(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f > s), RBool.NA)))
  def gt(l: RDouble, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: String) => boolean2Int(f.toString > s), RBool.NA)))

  def gt(l: RChar, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Int) => boolean2Int(f > s.toString), RBool.NA)))
  def gt(l: RChar, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Double) => boolean2Int(f > s.toString), RBool.NA)))
  def gt(l: RChar, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: String) => boolean2Int(f > s), RBool.NA)))

  def gteq(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f >= s), RBool.NA)))
  def gteq(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f.toDouble >= s), RBool.NA)))
  def gteq(l: RInt, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: String) => boolean2Int(f.toString >= s), RBool.NA)))

  def gteq(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f.toDouble >= s), RBool.NA)))
  def gteq(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f >= s), RBool.NA)))
  def gteq(l: RDouble, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: String) => boolean2Int(f.toString >= s), RBool.NA)))

  def gteq(l: RChar, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Int) => boolean2Int(f >= s.toString), RBool.NA)))
  def gteq(l: RChar, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Double) => boolean2Int(f >= s.toString), RBool.NA)))
  def gteq(l: RChar, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: String) => boolean2Int(f >= s), RBool.NA)))

  //todo equality of double and int can result in true in R, but false in Java
  def eql(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f == s), RBool.NA)))
  def eql(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f.toDouble == s), RBool.NA)))
  def eql(l: RInt, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: String) => boolean2Int(f.toString == s), RBool.NA)))

  def eql(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f.toDouble == s), RBool.NA)))
  def eql(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f == s), RBool.NA)))
  def eql(l: RDouble, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: String) => boolean2Int(f.toString == s), RBool.NA)))

  def eql(l: RChar, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Int) => boolean2Int(f == s.toString), RBool.NA)))
  def eql(l: RChar, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Double) => boolean2Int(f == s.toString), RBool.NA)))
  def eql(l: RChar, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: String) => boolean2Int(f == s), RBool.NA)))

  def neq(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f != s), RBool.NA)))
  def neq(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f.toDouble != s), RBool.NA)))
  def neq(l: RInt, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: String) => boolean2Int(f.toString != s), RBool.NA)))

  def neq(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f.toDouble != s), RBool.NA)))
  def neq(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f != s), RBool.NA)))
  def neq(l: RDouble, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: String) => boolean2Int(f.toString != s), RBool.NA)))

  def neq(l: RChar, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Int) => boolean2Int(f != s.toString), RBool.NA)))
  def neq(l: RChar, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: Double) => boolean2Int(f != s.toString), RBool.NA)))
  def neq(l: RChar, r: RChar): RBool = dim(l, r, RBool(modeIterate(l, r, (f: String, s: String) => boolean2Int(f != s), RBool.NA)))


  def andVec(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f != 0 && s != 0), RBool.NA)))
  def andVec(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f != 0 && s != .0), RBool.NA)))
  def andVec(l: RInt, r: RComplex): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Complex) => boolean2Int(f != 0 && s != 0), RBool.NA)))

  def andVec(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f != .0 && s != 0), RBool.NA)))
  def andVec(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f != .0 && s != .0), RBool.NA)))
  def andVec(l: RDouble, r: RComplex): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Complex) => boolean2Int(f != .0 && s != 0), RBool.NA)))

  def andVec(l: RComplex, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Complex, s: Int) => boolean2Int(f != 0 && s != 0), RBool.NA)))
  def andVec(l: RComplex, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Complex, s: Double) => boolean2Int(f != 0 && s != .0), RBool.NA)))
  def andVec(l: RComplex, r: RComplex): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Complex, s: Complex) => boolean2Int(f != 0 && s != 0), RBool.NA)))

  def orVec(l: RInt, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Int) => boolean2Int(f != 0 || s != 0), RBool.NA)))
  def orVec(l: RInt, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Double) => boolean2Int(f != 0 || s != .0), RBool.NA)))
  def orVec(l: RInt, r: RComplex): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Int, s: Complex) => boolean2Int(f != 0 || s != 0), RBool.NA)))

  def orVec(l: RDouble, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Int) => boolean2Int(f != .0 || s != 0), RBool.NA)))
  def orVec(l: RDouble, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Double) => boolean2Int(f != .0 || s != .0), RBool.NA)))
  def orVec(l: RDouble, r: RComplex): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Double, s: Complex) => boolean2Int(f != .0 || s != 0), RBool.NA)))

  def orVec(l: RComplex, r: RInt): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Complex, s: Int) => boolean2Int(f != 0 || s != 0), RBool.NA)))
  def orVec(l: RComplex, r: RDouble): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Complex, s: Double) => boolean2Int(f != 0 || s != .0), RBool.NA)))
  def orVec(l: RComplex, r: RComplex): RBool = dim(l, r, RBool(modeIterate(l, r, (f: Complex, s: Complex) => boolean2Int(f != 0 || s != 0), RBool.NA)))


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

