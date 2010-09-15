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

import model.RObject
import model.RVal.{RInt, RBool, RDouble}

/**
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */
object Sine extends StdBuiltin with ResultInDouble {

  def apply(r: RObject) = r match {
    case ri: RBool => applyForDouble(ri, (i: Int) => Math.sin(i toDouble))
    case ri: RInt => applyForDouble(ri, (i: Int) => Math.sin(i toDouble))
    case ri: RDouble => applyForDouble(ri, Math.sin(_: Double)) 
    case o => error("sine function cannot be applied to object of type " + o.`type`)
  }
}