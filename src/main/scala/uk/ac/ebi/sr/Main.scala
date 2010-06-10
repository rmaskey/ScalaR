package uk.ac.ebi.sr

import interpreter.RLexer
import model.{Type, RObject}

/**
 *
 * Date: 22.03.2010
 * @author Taalai Djumabaev
 */

object Main {


  def main(args: Array[String]) {
    println("Me and you")
    new RLexer
         new RObject { val objectType = Type.S4 }
    println("Me and you")
  }
}
