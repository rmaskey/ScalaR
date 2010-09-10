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
case object AsLogical extends StdBuiltin {
  import rutils.BoolCoercion._

  def `as.logical`(r: RObject): RBool = apply(r)

 //TODO coercions should be made properly. No attribute copying is done for now
  def apply(r: RObject): RBool = r match {
    case b: RBool => b
    case i: RInt => RBool(convertArray(i.s, int2Bool))
    case d: RDouble => RBool(convertArray(d.s, (e: Double) => double2Bool(e)))
    case c: RComplex => RBool(convertArray(c.s, (e: Complex) => complex2Bool(e)))
    case c: RChar => RBool(convertArray(c.s, (e: String) => char2Bool(e)))//in if - error("argument is not interpretable as logical")))
    case l: RList => (concatArrays(l.s.toSeq, apply)(RBool.m)).asInstanceOf[RBool]
    case _ => error("argument of type " + r.`type` + " is not interpretable as logical vector")
  }
}

case object AsInteger extends StdBuiltin {
  import rutils.IntCoercion._

  def `as.integer`(r: RObject) = apply(r)

  def apply(r: RObject): RInt = r match {
    case b: RBool => RInt(b.s) // unless we change the bool type
    case i: RInt => i
    case d: RDouble => RInt(convertArray(d.s, (e: Double) => double2Int(e)))
    case c: RComplex => RInt(convertArray(c.s, (e: Complex) => complex2Int(e)))
    case c: RChar => RInt(convertArray(c.s, (e: String) => char2Int(e)))
    case l: RList => (concatArrays(l.s.toSeq, `as.integer`)(RInt.m)).asInstanceOf[RInt]
    case _ => error("argument of type " + r.`type` + " is not interpretable as integer vector")
  }
}

case object AsDouble extends StdBuiltin {
  import rutils.DoubleCoercion._

  def `as.double`(r: RObject) = apply(r)

  def apply(r: RObject): RDouble = r match {
    case b: RBool => RDouble(convertArray(b.s, (e: Int) => e.toDouble))
    case i: RInt => RDouble(convertArray(i.s, (e: Int) => e.toDouble))
    case d: RDouble => d
    case c: RComplex => RDouble(convertArray(c.s, (e: Complex) => complex2Double(e)))
    case c: RChar => RDouble(convertArray(c.s, (e: String) => char2Double(e)))
    case l: RList => (concatArrays(l.s.toSeq, `as.double`)(RDouble.m)).asInstanceOf[RDouble]
    case _ => error("argument of type " + r.`type` + " is not interpretable as double vector")
  }
}

case object AsCharacter extends StdBuiltin {
  import rutils.CharCoercion._

  def `as.character`(r: RObject) = apply(r)

  def apply(r: RObject): RChar = r match {
    case b: RBool => RChar(convertArray(b.s, (e: Int) => int2Char(e)))
    case i: RInt => RChar(convertArray(i.s, (e: Int) => int2Char(e)))
    case d: RDouble => RChar(convertArray(d.s, (e: Double) => double2Char(e)))
    case c: RComplex => RChar(convertArray(c.s, (e: Complex) => complex2Char(e)))
    case c: RChar => c
    case l: RList => (concatArrays(l.s.toSeq, `as.character`)(RChar.m)).asInstanceOf[RChar]
    case _ => error("argument of type " + r.`type` + " is not interpretable as double vector")
  }
}