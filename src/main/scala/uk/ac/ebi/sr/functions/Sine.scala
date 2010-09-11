package uk.ac.ebi.sr
package functions

import model.RObject
import model.RVal.{RInt, RBool, RDouble}

/**
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */
object Sine extends StdBuiltin with ResultInDouble {

  def apply(r: RObject) = r match {
    case ri: RBool => applyF[Int](ri.s, (i: Int) => Math.sin(i toDouble))
    case ri: RInt => applyF[Int](ri.s, (i: Int) => Math.sin(i toDouble))
    case ri: RDouble => applyF[Double](ri.s, Math.sin(_)) 
    case o => error("sine function cannot be applied to object of type " + o.`type`)
  }
}