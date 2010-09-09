package uk.ac.ebi.sr
package functions

import model.{Environment, RObject}
import interpreter.{DeclArg, FDeclArg, NULL}

/**
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */

abstract class StdBuiltin(val X: String) extends Builtin {

  val params = List[FDeclArg](DeclArg(X))

  protected def apply(e: Environment) = e resolve X match {
    case Some(x) => process(x)
    case None => NULL
  }

  protected def process(r: RObject): RObject
}
