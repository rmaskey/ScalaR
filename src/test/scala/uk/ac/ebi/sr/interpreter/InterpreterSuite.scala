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
package interpreter

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import Helper._
import model.RVal.{RDouble, RInt}

/**
 *
 * Date: 01.06.2010
 * @author Taalai Djumabaev
 */

@RunWith(classOf[JUnitRunner])
class InterpreterSuite extends FunSuite {

  test("argument matching mechanism with environment bindings test ") {
    val input =
      """ y = 1000;
          z = function(x = 5, foo = {2 + 4}, fob, ...) { x + fob + foo + y };
          z(,,12) """
    // todo to change when printing is fixed
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(1023)))
  }

  test("argument matching mechanism test") {
    val input =
      """ x = function(e= 5, roo = 6, ros=100, ..., fy = 8) { e + roo + ros + fy};
          x(1, ro=12, ros=1000, q, w, e, r, y,t, fy=1) """
    // todo to change when printing is fixed
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(1014)))
  }

  test("argument evaluation, environment binding test") {
    val input =
      """ y = 1
          x = function(z, s = { y = 1000; y + 1}) {s + z}
          x(2)
          y
      """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(1000)))
  }

  test("environment LEXICAL binding test") {
    val input =
      """ a = 1
          f = function(x) { y <- 10; g <- function(x) x + y; g}; h <- f(1); h(3)
      """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(13)))
  }

  test("environment binding test") {
    val input =
      """ a = 1
          f = function(x) { g <- function(x) x; g}; h <- f(1); h(3)
      """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(3)))
  }

  test("argument evaluation, environment binding test 2") {
    val input =
      """ y = 1
          x = function(s = { y = 1000; y + 1}) {s}
          x()
          y
      """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(1000)))
  }

  test(" no arguments function test ") {
    val input =
    """
        x = 5                 ;
        y = function() 10 + x;
        x = 133+2;
        "y"()
    """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(145)))
  }

  test(" user defined operation test ") {
    val input =
    """ "%xyz%" = function(a, b) a + b
         1 %xyz% 2 + 2.5

    """
    assert(equalSeq(evaluate(input).asInstanceOf[RDouble], RDouble(5.5)))
  }

  test(" default arguments matching test") {
    val input =
    """ q = function(a = 1, b = 10) if (a < b) 0 else 100;
    """
    val tail1 = "q()"
    assert(equalSeq(evaluate(input + tail1).asInstanceOf[RInt], RInt(0)))
    val tail2 = "q(,)"
    assert(equalSeq(evaluate(input + tail2).asInstanceOf[RInt], RInt(0)))
    val tail3 = "q(, 1)"
    assert(equalSeq(evaluate(input + tail3).asInstanceOf[RInt], RInt(100)))
    val tail4 = "q(200)"
    assert(equalSeq(evaluate(input + tail4).asInstanceOf[RInt], RInt(100)))
    val tail5 = "q(200,)"
    assert(equalSeq(evaluate(input + tail5).asInstanceOf[RInt], RInt(100)))
    val tail6 = "q(b = 2, a = 3)"
    assert(equalSeq(evaluate(input + tail6).asInstanceOf[RInt], RInt(100)))
  }

  test (" default arguments matching bug fix ") {
    val input =
    """ q = function(a = 1, b = 10) if (a < b) 0 else 100
        q(b = 0)
    """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(100)))
  }
}