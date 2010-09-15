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

import rutils.NAs
import model.RVal.{RDouble, RInt, RChar, RBool}
import model.Sequential
import Operations.convertArray

/**
 * An alternative to the Sequential's applyF method where
 * the resulting object's type is same to the calling object type
 *
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */
trait ResultInDouble {

  def applyForDouble[A](seq: Sequential[A], f: A => Double) =
    RDouble(convertArray(seq.s, f, seq.NA, NAs.doubleNA))
}

trait ResultInInt {

  def applyForInt[A](seq: Sequential[A], f: A => Int) =
    RInt(convertArray(seq.s, f, seq.NA, NAs.intNA))
}

trait ResultInBool {

  def applyForBool[A](seq: Sequential[A], f: A => Int) =
    RBool(convertArray(seq.s, f, seq.NA, NAs.boolNA))
}

trait ResultInChar {

  def applyForChar[A](seq: Sequential[A], f: A => String) =
    RChar(convertArray(seq.s, f, seq.NA, NAs.charNA))
}