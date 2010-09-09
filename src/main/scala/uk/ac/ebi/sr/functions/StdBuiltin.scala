package uk.ac.ebi.sr.functions

import uk.ac.ebi.sr.model.{Environment, RObject}
import uk.ac.ebi.sr.interpreter.{DeclArg, FDeclArg, NULL}

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
