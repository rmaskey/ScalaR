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
    assert(CharCoercion.complex2Char(c) == NAs.charNA)
  }

}