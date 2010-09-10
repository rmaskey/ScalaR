package uk.ac.ebi.sr
package functions

import model.{Environment, RObject}
import interpreter.{DeclArg, FDeclArg, NULL}

/**
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */

abstract class StdBuiltin(val X: String) extends Builtin {

  def this() = this(StdBuiltin.X)

  val params = List[FDeclArg](DeclArg(X))

  protected def process(e: Environment) = e resolve X match {
    case Some(x) => apply(x)
    case None => NULL
  }

  protected def apply(r: RObject): RObject
}

object StdBuiltin {
  val X = "x"
}
