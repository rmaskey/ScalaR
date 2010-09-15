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
package rutils

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 *
 * Date: Jul 13, 2010
 * @author Taalai Djumabaev
 */

@RunWith(classOf[JUnitRunner])
class CoercionSuite extends FunSuite {

  test("NA coercion test ") {
    val char = NAs.charNA
    val i = IntCoercion.char2Int(char)
    assert(i == NAs.intNA)
    val d = DoubleCoercion.int2Double(i)
    assert(d == NAs.doubleNA)
    val c = ComplexCoercion.double2Complex(d)
    assert(CharCoercion.int2Char(i) == NAs.charNA)
  }

}