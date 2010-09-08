package uk.ac.ebi.sr
package functions

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import model.RVal._
import functions._

import Helper._
/**
 *
 * Date: Jul 7, 2010
 * @author Taalai Djumabaev
 */

@RunWith(classOf[JUnitRunner]   )
class OperationsSuite extends FunSuite {

  test("length assignment test ") {
    val input = " x <- 1: 10; length(x) <- 3; x "

    //println(Length.`length<-`(RInt(Array.tabulate(10)(_.toInt)), 7))
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(1, 2, 3)))
  }

  test("rlist element extraction test") {
    val input =
      """
        l = list(1,2,3,4); attr(l, "names") <- c("a", "sec", "er");  l$"a"
      """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(1)))
  }

  test("rlist element extraction test 2") {
    val input =
      """
        l = list(1,2,3,4); attr(l, "names") <- c("a", "sec", "er");  l$"s"
      """
    assert(equalSeq(evaluate(input).asInstanceOf[RInt], RInt(2)))
  }
}
