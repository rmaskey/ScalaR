package uk.ac.ebi.sr.functions

import uk.ac.ebi.sr.model.{RObject, Sequential}
import collection.mutable.ArrayBuffer
import AsInteger._
import uk.ac.ebi.sr.interpreter.{EmptyIndex, NULL}
import uk.ac.ebi.sr.model.RVal._

/**
 *
 * Date: Jul 9, 2010
 * @author Taalai Djumabaev
 */

object Subset {

  def `[`(l: RObject, dimSubset: List[RObject]): RObject = {
    if (isMultidimensional(l) && dimSubset.size > 1) NULL //todo multidimensional case
    else if (dimSubset.size > 1) error("wrong number of dimensions for " + l)
    else l match {
      case i: RBool => oneDimensional(i, dimSubset.head)
      case i: RInt => oneDimensional(i, dimSubset.head)
      case i: RDouble => oneDimensional(i, dimSubset.head)
      case i: RChar => oneDimensional(i, dimSubset.head)
      case i: RComplex => oneDimensional(i, dimSubset.head)
      //case seq: Sequential[Any] =>
      case o => error(o.`type` + " is not subsettable")
    }
  }



  def oneDimensional[B](seq: Sequential[B], index: RObject) = index match {
    case EmptyIndex => seq
    case b: RBool => seq.applyF(subsetLogical(seq, b)(seq.m))
    case i: RInt => isOfOneSign(i) match {
      case (true, true) => NULL // todo R retruns integer(0)
      case (false, false) => error("only 0's may be mixed with negative subscripts")
      case (true, false) => seq.applyF(subset(seq, i)(seq.m))
      case (false, true) => seq.applyF(subsetNegation(seq, i)(seq.m))
    }
    case i: RDouble =>
      val asInt = `as.integer`(i)
      isOfOneSign(asInt) match {
        case (true, true) => NULL // todo R retruns integer(0)
        case (false, false) => error("only 0's may be mixed with negative subscripts")
        case (true, false) => seq.applyF(subset(seq, asInt)(seq.m))
        case (false, true) => seq.applyF(subsetNegation(seq, asInt)(seq.m))
      }
    case i: RChar => NULL // todo takes by names attribute
    case o => error("invalid subscript type: " + o.`type`)
  }

  def isMultidimensional(o: RObject) = o.attr(Attr.DIM) match {
    case s: Sequential[Any] => s.length <= 1
    case _ => true
  }

  def isOfOneSign(ri: RInt): Pair[Boolean, Boolean] = {
    var i = 0
    var pos = true
    var neg = true

    val len = ri.s.length
    while (i < len && (pos || neg)) {
      pos = ri.s(i) >= 0 && pos
      neg = ri.s(i) <= 0 && neg
      i += 1
    }
    (pos, neg)
  }

  def subset[B](b: Sequential[B], ind: RInt)(implicit m: Manifest[B]) = {
    val a = b.s
    val index = ind.s
    val indLen = index.length
    val len = a.length
    val buf = new ArrayBuffer[B](indLen) //zero indexes can be wastefull here
    var i = 0
    while (i < indLen) {
      val in = index(i)
      if (in == 0) {} else if (in >= len || in == ind.NA) buf += b.NA
      else buf += a(in - 1) //since index in R starts with 1
      i += 1
    }
    buf.toArray.asInstanceOf[Array[B]]
  }

  def subsetNegation[B](b: Sequential[B], ind: RInt)(implicit m: Manifest[B]) = {
    val a = b.s
    val len = a.length
    val index = ind.applyChange(java.util.Arrays.sort).s

    val buf = new ArrayBuffer[B]()
    var i = 0
    var j = index.length - 1
    while (index(j) == 0) j -=1      //ignore zeros
    while (i < len) {
      val negated = -(index(j) + 1)  //since index in R starts with 1
      if (negated == i) j -=1
      else buf += a(i) //no need to check for NA, since index array contains only negative integers and cannot be NA
      i += 1
    }
    buf.toArray
  }

  def subsetLogical[B](b: Sequential[B], ind: RBool)(implicit m: Manifest[B]) = {
    val a = b.s
    val len = a.length
    val index = ind.s
    val indLen = index.length //todo warning if index is shorter and not a multiple of len

    val count = math.max(len, indLen)
    val buf = new ArrayBuffer[B](count)
    var i = 0
    var j = 0
    while(i < count) {
      index(j) match {
        case 0 => if (i >= len) buf += b.NA
        case 1 => if (i >= len) buf += b.NA else buf += a(i)
        case ind.NA => buf += b.NA
        case a => error("undefined value for boolean " + a); // can't get here
      }
      if (j >= indLen - 1) j = 0 else j += 1
      i += 1
    }
    buf.toArray
  }
}
