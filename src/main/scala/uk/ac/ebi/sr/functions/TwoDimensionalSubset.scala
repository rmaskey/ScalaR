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

import rutils.NAs
import model.{Sequential, RObject}
import model.RVal.{RBool, RInt, RDouble}
import collection.mutable.ArrayBuffer
import AsInteger._
import interpreter.EmptyIndex

/**
 *
 * Date: Jul 26, 2010
 * @author Taalai Djumabaev
 */

object TwoDimensionalSubset {

  def apply[B](seq: Sequential[B], x: Int, y: Int, ind: Pair[RObject, RObject]) =
    new TwoDimensionalSubset(seq, x, y)(seq.m).`[`(ind)

  def main(args: Array[String]) {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    ri.`attr<-`("dim", RInt(2, 5))
    //val in = (RInt(3, 2, 1), RInt(-1, -2))
    val in = (RInt(-1), RInt(3, 2, 5, 1))
    println(TwoDimensionalSubset(ri, 2, 5, in))
  }
}

class TwoDimensionalSubset[A](val seq: Sequential[A], val x: Int, val y: Int)(implicit val m: Manifest[A]) {
  import Subset.isOfOneSign

  def `[`(ind: Pair[RObject, RObject]) = {
    val buf = new ArrayBuffer[A]()
    val f = getXInd(ind._1, buf)
    val fun = ind._2 match {
      case b: RBool => yBool(b.s, buf, f)
      case i: RInt => isOfOneSign(i) match {
        case (true, false) => yInt(i.s, buf, f)
        case (false, true) => yNegInt(i, buf, f)
        case _ => error("only 0's may be mixed with negative subscripts")
      }
      case d: RDouble =>
        val i = AsInteger(d)
        isOfOneSign(i) match {
          case (true, false) => yInt(i.s, buf, f)
          case (false, true) => yNegInt(i, buf, f)
          case _ => error("only 0's may be mixed with negative subscripts")
        }
      case EmptyIndex => yAll(buf, f)
      case o => error("invalid subscript type: " + o.`type`)
    }
    seq.applyF(fun)
  }

  private def getXInd(o: RObject, buf: ArrayBuffer[A]): (Int => Unit) = o match {
    case b: RBool => xBool(b.s, buf)
    case i: RInt => isOfOneSign(i) match {
      case (true, false) => xInt(i.s, buf)
      case (false, true) => xNegInt(i, buf)
      case _ => error("only 0's may be mixed with negative subscripts")
    }
    case d: RDouble =>
      val i = AsInteger(d)
      isOfOneSign(i) match {
        case (true, false) => xInt(i.s, buf)
        case (false, true) => xNegInt(i, buf)
        case _ => error("only 0's may be mixed with negative subscripts")
      }
    case EmptyIndex => xAll(buf)
    case o => error("invalid subscript type: " + o.`type`)
  }

  private def yBool(b: Array[Int], buf: ArrayBuffer[A], f: Int => Unit) = {
    val lenDecr = b.length - 1
    if (lenDecr >= y) error("subscript out of bounds")
    var i = 0
    var j = 0
    while(i < y) {
      b(j) match {
        case 1 => f(i)
        case NAs.boolNA => f(NAs.intNA)
        case _ => // do nothing
      }
      if (j >= lenDecr) j = 0 else j += 1
      i += 1
    }
    buf.toArray
  }

  private def xBool(b: Array[Int], buf: ArrayBuffer[A])(i: Int) = {
    val lenDecr = b.length - 1
    if (lenDecr >= x) error("subscript out of bounds")
    val a = seq.s
    var j = 0
    var k = 0
    while (j < x) {
      b(k) match {
        case 1 => if (i == NAs.intNA) buf += seq.NA else buf += a(i * x + j)
        case NAs.boolNA => buf += seq.NA
        case _ => // do nothing
      }
      if (k >= lenDecr) k = 0 else k += 1
      j += 1
    }
  }

  private def yInt(b: Array[Int], buf: ArrayBuffer[A], f: Int => Unit) = {
    val len = b.length
    var i = 0
    while(i < len) {
      b(i) match {
        case NAs.intNA => f(NAs.intNA)
        case 0 =>
        case value => if (value > y) error("subscript out of bounds") else f(value - 1)
      }
      i += 1
    }
    buf.toArray
  }

  private def xInt(b: Array[Int], buf: ArrayBuffer[A])(i: Int) = {
    val len = b.length
    val a = seq.s
    val isNA = i == NAs.intNA
    var j = 0
    while (j < len) {
      b(j) match {
        case NAs.intNA => buf += seq.NA
        case 0 =>
        case value =>
          if (value > x) error("subscript out of bounds")
          else if (isNA) buf += seq.NA
          else buf += a(i * x + value - 1)
      }
      j += 1
    }
  }

  private def yNegInt(ind: RInt, buf: ArrayBuffer[A], f: Int => Unit) = {
    val index = ind.applyChange(java.util.Arrays.sort).s
    var i = 0
    var j = index.length - 1
    while (index(j) == 0) j -= 1      //ignore zeros
    while (i < y) {
      val negated = if (j < 0) -1 else -(index(j) + 1)  //since index in R starts with 1
      if (negated == i) j -= 1
      else f(i) //no need to check for NA, since index array contains only negative integers and cannot be NA
      i += 1
    }
    buf.toArray
  }

  private def xNegInt(ind: RInt, buf: ArrayBuffer[A])(i: Int) = {
    val index = ind.applyChange(java.util.Arrays.sort).s
    val a = seq.s
    var j = 0
    var k = index.length - 1
    while (index(k) == 0) k -= 1      //ignore zeros
    while (j < x) {
      val negated = if (k < 0) -1 else -(index(k) + 1)  //since index in R starts with 1
      if (negated == j) k -= 1
      else if (i == NAs.intNA) buf += seq.NA
      else buf += a(i * x + j)
      j += 1
    }
  }

  private def yAll(buf: ArrayBuffer[A], f: Int => Unit) = {
    var i = 0
    while (i < y) {
      f(i)
      i += 1
    }
    buf.toArray
  }

  private def xAll(buf: ArrayBuffer[A])(i: Int) = {
    val a = seq.s
    var j = 0
    while (j < x) {
      buf += a(i * x + j)
      j += 1
    }
  }
}

