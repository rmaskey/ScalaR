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

import interpreter.{Interpreter, Expression, RParser}
import model.Sequential

/**
 *
 * Date: Jul 16, 2010
 * @author Taalai Djumabaev
 */

object Helper {

  def evaluate(input: String): Any = {
    RParser.parseUnwrap(input) match {
      case e: Expression => Interpreter.interpret(e, baseEnv)._1
      case _ => None
    }
  }


  def equalSeq[A, B](a: Sequential[A], b: Sequential[B]): Boolean = {
    if (a == b) return true
    if (a.length != b.length) return false
    for ((i, j) <- a.s.zip(b.s)) if (i != j) return false
    a.attributes == b.attributes
  }

  val baseEnv = RSession.baseEnv
}