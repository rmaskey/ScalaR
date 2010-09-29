/*
 * Copyright (c) 2009-2010 European Molecular Biology Laboratory
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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

  protected def process(env: Environment): RObject = env.resolve(X) match {
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
