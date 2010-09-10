package uk.ac.ebi.sr
package functions

import model.{Environment, RObject}
import interpreter._

/**
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
trait Assignable extends ArgMatching {
  def `<-`(args: List[FCallArg], fEnv: Environment, newValue: RObject): RObject = {
    //it is never empty list
    val name = args.head match {
      case CallArg(Var(i)) => i
      case CallArg(Lit(i)) => i
      case CallArgDef(n, Var(i)) => i
      case CallArgDef(n, Lit(i)) => i
      case NoneArg => error("invalid (NULL) left side of assignment")
      case _ => error("target of assignment expands to non-language object")
    }
    val newEnv = evalArgs(args, fEnv, fEnv)
    val res = assign(newEnv, newValue)
    fEnv.resolveWithEnv(name) match {
      case Some((o, e)) => if (o != res) {
        o.removeReferencer
        e += (name, res)
      }
      case None => //can't get here
    }
    if (!newEnv.isBound) newEnv.cleanAll
    res
  }

  protected def assign(env: Environment, newValue: RObject): RObject
}