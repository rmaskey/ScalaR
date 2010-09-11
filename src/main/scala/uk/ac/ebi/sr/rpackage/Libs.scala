package uk.ac.ebi.sr
package rpackage

import functions.Builtin
import interpreter.{NULL, DeclArg, FDeclArg}
import model.{RObject, Environment}
import model.RVal.RChar

/**
 *
 * Date: Sep 7, 2010
 * @author Taalai Djumabaev
 */

object Libs extends Builtin {

  val DIR = "dir"
  val params = List[FDeclArg](DeclArg(DIR))

  protected def process(env: Environment): RObject = env.resolve(DIR) match {
    case Some(s: RChar) => {
      if (s.length == 1) addLibsDir(s.s(0))
      //todo warnings otherwise
      NULL
    }
    case _ => NULL
  }

  def addLibsDir(s: String) = RSession.libs += s
}