package uk.ac.ebi.sr
package functions

import collection.mutable.ArrayBuffer
import AsInteger._
import model.RVal._
import model.{Environment, RObject, Sequential}
import interpreter.{Var, Lit, EmptyIndex, NULL}
import rutils.NAs

/**
 *
 * Date: Jul 9, 2010
 * @author Taalai Djumabaev
 */
object Subset {

  def `[`[A](seq: Sequential[A], dimSubset: List[RObject])(implicit m: Manifest[A]): RObject =
    (seq.attr(Attr.DIM), dimSubset.size) match {
      case (s: Sequential[Int], 1) =>
        val res = OneDimensionalSubset.`[`(seq, dimSubset.head)
        res.dropAttr(Attr.DIM, Attr.DIM_NAMES) //drops the attributes unsafely. no reference counting needed.
        res
      case (s: Sequential[Int], 2) => 
        if (s.length != 2) error("incorrect number of dimensions")
        else TwoDimensionalSubset(seq, s.s(0), s.s(1), (dimSubset.head, dimSubset.last)) 
      case (s: Sequential[Int], _) => multiDimensional(seq, dimSubset)
      case (NULL, 1) => OneDimensionalSubset.`[`(seq, dimSubset.head)
      case _ => error("incorrect number of dimensions for " + seq)
  }

  def `[<-`(ob: RObject, dimSubset: List[RObject], env: Environment, newValue: RObject): RObject = {
    val name = ob match {
      case Var(i) => i
      case Lit(i) => i
      case _ => error("could not find [<- function for given context")
    }
    env.resolveWithEnv(name) match {
      case Some((o, e)) =>
        if (isMultidimensional(o) && dimSubset.size > 1) NULL // todo multidimensional case
        else if (dimSubset.size > 1) error("wrong number of dimensions for " + name)
        else {
          val res = (o, newValue) match {
            case (s: Sequential[Any]) => {
              NULL
            }
            case (obj, _) => error(obj.`type` + " is not subsettable")
          }
          o.removeReferencer
          e += (name, res)
        }
      case None => //can't get here
    }
    NULL
  }

  def oneDimensionalAssignment[B](seq: Sequential[B], index: RObject, newValue: RObject) = {
    if (newValue.`type` == seq.`type`)
    newValue match {

      case n: Sequential[B] =>
      case _ =>
    }
  }

  def multiDimensional[B](seq: Sequential[B], index: List[RObject])(implicit m: Manifest[B]) = {
    val dim: Array[Int] = seq.attr(Attr.DIM) match {
      case s: RInt => s.s
      case _ => error("internal error: no dim attribute")
    }
    val len = dim.length
    if (index.size != len) error("incorrect number of dimensions")
    val buf = new ArrayBuffer[Array[Int]](len)
    var j = 0
    var resultLen = 1
    for (ob <- index) {
      val dimj = dim(j)
      val add = ob match {
        case b: RBool => if (b.length > dimj) error("(subscript) logical subscript too long") else extendBool(b, dimj)
        case i: RInt =>
          isOfOneSign(i) match {
            case (true, false) => if (i.forall((a: Int) => a <= dimj)) i.s else error("subscript out of bounds")
            case (false, true) => negateInd(i, dimj)
            case _ => error("only 0's may be mixed with negative subscripts")
          }
        case d: RDouble =>
          val i = `as.integer`(d)
          isOfOneSign(i) match {
            case (true, false) => if (i.forall((a: Int) => a <= dimj)) i.s else error("subscript out of bounds")
            case (false, true) => negateInd(i, dimj)
            case _ => error("only 0's may be mixed with negative subscripts")
          }
        case EmptyIndex => Array.tabulate[Int](dimj)(_ + 1)
        case o => error("invalid subscript type: " + o.`type`)
      }
      buf += add
      resultLen *= add.length
      j += 1
    }
    seq.applyF(subsetMultiple(seq, multiples(dim), buf.toArray, resultLen))
  }

  def extendBool(b: RBool, size: Int) = {
    val buf = new ArrayBuffer[Int](size)
    val a = b.s
    var i = 0
    var j = 0
    while (i < size) {
      a(j) match {
        case 1 => buf += i + 1
        case NAs.boolNA => NAs.intNA
        case _ =>  // do nothing
      }
      i += 1
      if (j >= a.length - 1) j = 0 else j += 1
    }
    buf.toArray
  }

  def negateInd(ri: RInt, dim: Int) = {
    val buf = new ArrayBuffer[Int]()
    val index = ri.applyChange(java.util.Arrays.sort).s
    var i = 0
    var j = index.length - 1
    while (index(j) == 0) j -=1      //ignore zeros
    while (i < dim) {
      val negated = if (j < 0) -1 else -(index(j) + 1)  //since index in R starts with 1
      if (negated == i) j -=1
      else buf += i + 1 //no need to check for NA, since index array contains only negative integers and cannot be NA
      i += 1
    }
    buf.toArray
  }


  def isMultidimensional(o: RObject) = o.attr(Attr.DIM) match {
    case s: Sequential[Any] => s.length > 1
    case _ => false
  }

  def isOfOneSign(ri: RInt): Pair[Boolean, Boolean] = {
    var i = 0
    var pos = true
    var neg = true

    val len = ri.s.length
    while (i < len && (pos || neg)) {
      pos = pos && ri.s(i) >= 0
      neg = neg && ri.s(i) <= 0
      i += 1
    }
    (pos, neg)
  }

  def subsetMultiple[B](b: Sequential[B], mul: Array[Int], index: Array[Array[Int]], resultLen: Int)(implicit m: Manifest[B]) = {
    val a = b.s
    var i = 0
    var len = mul.length
    val indMul = indMultiples(index)
    Array.fill[B](resultLen)({
      var m = i
      var j = len
      val subInd = Array.fill(len)({
        j -= 1
        val res = m / indMul(j) // todo easier way like /%
        m = m % indMul(j)
        res
      })
      i +=1
      a(ind2Value(mapIndexes(subInd, index), mul))
    })
  }

  def mapIndexes(subInd: Array[Int], index: Array[Array[Int]]) = {
    var i = -1
    var j = subInd.length
    Array.fill(subInd.length)({
      i += 1
      j -= 1
      index(i)(subInd(j))
    })
  }

  def indMultiples(index: Array[Array[Int]]) = {
    var i = -1
    var mul = 1
    Array.fill(index.length)({
      if (i >= 0) mul *= index(i).length
      i += 1
      mul
    })
  }

  def multiples(dim: Array[Int]) = {
    var multiple = 1
    var j = -1
    Array.fill(dim.length)({
      if (j >= 0) multiple *= dim(j)
      j += 1
      multiple
    })
  }

  def ind2Value(ind: Array[Int], multiples: Array[Int]) = {
    require(ind.length == multiples.length)
    var res = 0
    val len = ind.length
    var i = 0
    while (i < len) {
      res += (ind(i) - 1) * multiples(i)
      i += 1
    }
    res
  }



  def main(args: Array[String]) = {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(2, 5, 2))
    val in: List[RObject] = List(RInt(1), RInt(-3), RInt(1))
    println(`[`(ri, in))
  }

  def printArray[B](a: Array[B]) {
    var i = 0
    val len = a.length
    println("a: ") 
    while (i < len) {
      a(i) match {
        case aa: Array[B] => print("["); printArray(aa); print("]")
        case aa => print(aa + " ")
      }
      i += 1
    }
    println
  }
}


