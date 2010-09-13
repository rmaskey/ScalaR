package uk.ac.ebi.sr
package functions

import interpreter.{DeclArg, FDeclArg, NULL}
import model.{Sequential, RObject, Environment}
import model.RVal.RInt

/**
 *
 * Date: Sep 8, 2010
 * @author Taalai Djumabaev
 */
object Print extends Builtin {
  val PRINT_WIDTH = 20

  val X = "x"
  val params = List[FDeclArg](DeclArg(X))

  protected def apply(env: Environment): RObject = env.resolve(X) match {
    case Some(r) => print(r)
    case _ => NULL
  }

  def print(r: RObject) = r match {
    case s: Sequential[_] => NULL
    case o => NULL
  }

  def printRowStart(row: Int) = "[" + row + "] "

  def printSeq[A](s: Array[A], tail: => String) = {
    val buf = attr(Attr.DIM) match {
      case NULL => oneDimPrint(s)
      case i: RInt => new StringBuilder// todo multiDimPrint(s, i.s)
      case o => error(" Dim attribute should be an integer vector ")
    }
    buf append tail
    buf toString
  }

  def oneDimPrint[A](s: Array[A]): StringBuilder = {
    val buf = new StringBuilder
    val length = s.length
    var i = 0
    var j = 0
    var row = 1
    while (i < length) {
      if (i == j) {
        buf append "\n"
        buf append printRowStart(row)
        row += 1
        j += PRINT_WIDTH
      }
      buf append "  "
      buf append s(i)
      i += 1
    }
    buf
  }
}