package uk.ac.ebi.sr
package functions

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import model.RVal._
import rutils.NAs
import model.Sequential

import Helper._
/**
 *
 * Date: Jul 16, 2010
 * @author Taalai Djumabaev
 */

@RunWith(classOf[JUnitRunner])
class SubsetTestSuite extends FunSuite {


  test("simplest subset test ") {
    val rd = RDouble(1, 1.4, 12, 13.8, 7)
    val indInt = RInt(1, 2, NAs.intNA, 5)
    assert(equalSeq(Subset.`[`(rd, List(indInt)).asInstanceOf[RDouble], RDouble(1.0, 1.4, NAs.doubleNA, NAs.doubleNA)))

    val indBool = RBool(1, 0, NAs.intNA)
    assert(equalSeq(Subset.`[`(rd, List(indBool)).asInstanceOf[RDouble], RDouble(1.0, NAs.doubleNA, 13.8)))
  }

  test("simplest subset using interpreter ") {
    val input =
      """ x = 1:10
          x[1.2:3.8]
      """
    assert(evaluate(input).toString == "List(1, 2, 3)")
  }

}