package uk.ac.ebi.sr
package functions

import model.{RObject, Environment, Sequential}
import model.RVal.RInt
import interpreter.{NULL, DeclArg, FDeclArg}

/**
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
case object Length extends StdBuiltin with Assignable {
  val zeroLength = RInt(0)

  protected def assign(env: Environment, newValue: RObject) = {
    if (!newValue.isInstanceOf[RInt]) error("invalid value for length")
    env.resolve(X) match {
      case Some(r: Sequential[_]) => {
        val nv = AsInteger(newValue)
        if (nv.length != 1) error("invalid argument for length ")
        val len = nv.s(0)
        if (len < 0) error("vector length cannot be negative")
        val res = `length<-`(r, len)(r.m)
        res
      }
      case Some(r) => error("invalid argument for length assignment")
      case _ => NULL
    }
  }

  def `length<-`[C](r: Sequential[C], nv: Int)(implicit m: Manifest[C]) = {
    if (nv == r.s.length) r
    else r.applyF({
      val newSeq = Array.tabulate[C](nv)(_ => r.NA)
      r.s.copyToArray(newSeq, 0, math.min(r.s.length, nv))
      newSeq
    })                     //todo no attrbute copying is done
  }

  def apply(r: RObject) = RInt(r match {
    case s: Sequential[_] => s.length
    case _ => 0
  })
}