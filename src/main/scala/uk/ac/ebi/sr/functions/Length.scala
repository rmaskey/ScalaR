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

import model.{RObject, Environment, Sequential}
import model.RVal.RInt
import interpreter.{NULL, DeclArg, FDeclArg}

/**
 * function evaluating the length of the object. If object has no length 0 is returned
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
case object Length extends StdBuiltin with Assignable {
  val zeroLength = RInt(0)

  protected def assign(env: Environment, newValue: RObject) = {
    if (!newValue.isInstanceOf[RInt]) error("invalid value for length")
    env.resolve(X) match {
      case Some(r: Sequential[_]) => {
        val nv = AsInteger(newValue)
        if (nv.length != 1) error("invalid argument for length ")
        val len = nv.s(0)
        if (len < 0) error("vector length cannot be negative")
        val res = `length<-`(r, len)(r.m)
        res
      }
      case Some(r) => error("invalid argument for length assignment")
      case _ => NULL
    }
  }

  def `length<-`[C](r: Sequential[C], nv: Int)(implicit m: Manifest[C]) = {
    if (nv == r.s.length) r
    else r.applyF({
      val newSeq = Array.tabulate[C](nv)(_ => r.NA)
      r.s.copyToArray(newSeq, 0, math.min(r.s.length, nv))
      newSeq
    })                     //todo no attrbute copying is done
  }

  def apply(r: RObject) = RInt(r match {
    case s: Sequential[_] => s.length
    case _ => 0
  })
}