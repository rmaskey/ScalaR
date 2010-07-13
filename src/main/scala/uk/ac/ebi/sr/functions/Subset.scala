package uk.ac.ebi.sr.functions

import uk.ac.ebi.sr.model.{RObject, Sequential}
import uk.ac.ebi.sr.model.RVal.{RChar, RDouble, RBool, RInt}
import uk.ac.ebi.sr.interpreter.{EmptyIndex, NULL, IndexArgument, Expression}

/**
 *
 * Date: Jul 9, 2010
 * @author Taalai Djumabaev
 */

object Subset {

  def `[`(l: RObject, dimSubset: List[RObject]): RObject = {
    if (isMultidimensional(l) && dimSubset.size > 1) NULL //todo
    else if (dimSubset.size > 1) error("wrong number of dimensions for " + l)
    else l match {
      case s: Sequential[Any] => {
        dimSubset.head match {
          case EmptyIndex => l
          case b: RBool =>
          case i: RInt =>
          case i: RDouble =>
          case i: RChar =>
          case o => error("invalid subscript type: " + o.`type`)
        }
        NULL
      }
      case o: RObject => error(o.`type` + " is not subsettable")
    }
  }

  def isMultidimensional(o: RObject) = o.attr(Attr.DIM) match {
    case s: Sequential[Any] => s.length <= 1
    case _ => true
  }
}
