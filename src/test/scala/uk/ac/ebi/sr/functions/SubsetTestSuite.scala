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

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import model.RVal._
import rutils.NAs._
import Helper._
import model.{RObject, Sequential}
import interpreter.EmptyIndex
import Subset._
import org.junit.Before

/**
 *
 * Date: Jul 16, 2010
 * @author Taalai Djumabaev
 */
@RunWith(classOf[JUnitRunner])
class SubsetTestSuite extends FunSuite {


  test("simplest onedimensional subset test ") {
    val rd = RDouble(1, 1.4, 12, 13.8, 7)
    val indInt = RInt(1, 2, intNA, 5)
    assert(equalSeq(`[`(rd, List(indInt)).asInstanceOf[RDouble], RDouble(1.0, 1.4, doubleNA, 7.0)))

    val indBool = RBool(1, 0, intNA)
    assert(equalSeq(`[`(rd, List(indBool)).asInstanceOf[RDouble], RDouble(1.0, doubleNA, 13.8)))
  }


  test(" onedimensional negation test ") {
    val rd = RDouble(1, 1.4, 12, 13.8, 7)
    val indInt = RInt(-1, -2)
    assert(equalSeq(`[`(rd, List(indInt)).asInstanceOf[RDouble], RDouble(12, 13.8, 7.0)))
  }

  test("multidim subset test int") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(2, 5, 2))
    val in: List[RObject] = List(RInt(1), RInt(3, 2, 5), EmptyIndex)
    assert(equalSeq(`[`(ri, in).asInstanceOf[RInt], RInt(5, 3, 9, 15, 13, 19)))
  }

  test("multidim subset test bool") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(2, 5, 2))
    val in: List[RObject] = List(EmptyIndex, RBool(1, 0, 1), RDouble(1.34))
    assert(equalSeq(`[`(ri, in).asInstanceOf[RInt], RInt(1, 2, 5, 6, 7, 8)))

    val in2: List[RObject] = List(RDouble(1.2322323), RBool(1, 0, 1), RInt(1))
    assert(equalSeq(`[`(ri, in2).asInstanceOf[RInt], RInt(1, 5, 7)))
  }

  test("multidim subset test negate") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(2, 5, 2))
    val in: List[RObject] = List(RInt(1), RInt(-3, -5, 0, -2), EmptyIndex)
    val in2: List[RObject] = List(RInt(1), RInt(-3, -5, -2), EmptyIndex)
    val in3: List[RObject] = List(RInt(1), RInt(-3, -5, -2, 0, -100), EmptyIndex)
    assert(equalSeq(`[`(ri, in).asInstanceOf[RInt], RInt(1, 7, 11, 17)))
    assert(equalSeq(`[`(ri, in2).asInstanceOf[RInt], RInt(1, 7, 11, 17)))
    assert(equalSeq(`[`(ri, in3).asInstanceOf[RInt], RInt(1, 7, 11, 17)))
  }

  test (" multidim index negation bug fix ") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(2, 5, 2))
    val in: List[RObject] = List(RInt(1), RInt(-3), RInt(1))
    assert(equalSeq(`[`(ri, in).asInstanceOf[RInt], RInt(1, 3, 7, 9)))
  }

  test("simplest subset using interpreter ") {
    val input =
      """ x = 1:10
          x[1.2:3.8]
      """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(1, 2, 3)))
  }

  test("2 dimensional call from multidim subset test ") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    ri.`attr<-`("dim", RInt(2, 5))
    val in: List[RObject] = List(RInt(1), RInt(-3))
    assert(equalSeq(`[`(ri, in).asInstanceOf[RInt], RInt(1, 3, 7, 9)))
  }

  test("2 dimensional subset test 1 ") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(4, 5))

    val in = (RBool(0, 1), RBool(1, 0, 1))
    assert(equalSeq(TwoDimensionalSubset(ri, 4, 5, in).asInstanceOf[RInt], RInt(2, 4, 10, 12, 14, 16)))

    val in2 = (RBool(1, 1, 0), RInt(4, 1, 3))
    assert(equalSeq(TwoDimensionalSubset(ri, 4, 5, in2).asInstanceOf[RInt], RInt(13, 14, 16, 1, 2, 4, 9, 10, 12)))
  }

  test("2 dimensional subset test 2") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(4, 5))
    val in = (RInt(1), RInt(3, 2, 5, 1))
    assert(equalSeq(TwoDimensionalSubset(ri, 4, 5, in).asInstanceOf[RInt], RInt(9, 5, 17, 1)))

    val in2 = (RInt(1, 3), RBool(1, 0, 1))
    assert(equalSeq(TwoDimensionalSubset(ri, 4, 5, in2).asInstanceOf[RInt], RInt(1, 3, 9, 11, 13, 15)))
  }

  test ("2 dimensional subset bug fix") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(4, 5))

    val in = (RBool(1, 1, 0), RInt(0, 0, 3))
    assert(equalSeq(TwoDimensionalSubset(ri, 4, 5, in).asInstanceOf[RInt], RInt(9, 10, 12)))
    val in2 = (RInt(0, 1, 2, 3), RInt(0, 0, 3))
    assert(equalSeq(TwoDimensionalSubset(ri, 4, 5, in2).asInstanceOf[RInt], RInt(9, 10, 11)))    
  }

  test(" 2 dimensional subset test NA") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    ri.`attr<-`("dim", RInt(4, 5))
    val in = (RInt(1, 2, 3, intNA), RInt(intNA, 3))
    assert(equalSeq(TwoDimensionalSubset(ri, 4, 5, in).asInstanceOf[RInt], RInt(intNA, intNA, intNA, intNA, 9, 10, 11, intNA)))
  }

  test(" 2 dimensional negation") {
    val ri = RInt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    ri.`attr<-`("dim", RInt(2, 5))
    val in = (RInt(-1, -2), RInt(3, 2, 5, 1))
    val in2 = (RInt(-1), RInt(3, 2, 5, 1))
    assert(TwoDimensionalSubset(ri, 2, 5, in).asInstanceOf[RInt].isEmpty)
    assert(equalSeq(TwoDimensionalSubset(ri, 2, 5, in2).asInstanceOf[RInt], RInt(6, 4, 10, 2)))
  }

  test(" 2 dimensional subset from interpreter") {
    val input =
      """ x = 1:20
          attr(x, "dim") <- 4:5
          x[, 4:3]
      """
    try {
      assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(13, 14, 15, 16, 9, 10, 11, 12)))
    } catch {
      case e: Exception => e.printStackTrace
    }
  }
}