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

import functions.{TwoDimensionalSubset, Operations, Subset}
import collection.mutable.ArrayBuffer
import interpreter.{Expression, Interpreter, RParser, RLexer}
import model.{Type, RObject}
import model.RVal._
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq
import swing.FileChooser
import javax.swing.JFrame

/**
 *  ####    ####     ##    #         ##    #####
 * #       #    #   #  #   #        #  #   #    #
 *  ####   #       #    #  #       #    #  #    #
 *      #  #       ######  #       ######  #####
 * #    #  #    #  #    #  #       #    #  #   #
 *  ####    ####   #    #  ######  #    #  #    #
 *
 *
 * Date: 22.03.2010
 * @author Taalai Djumabaev
 */

object Main {
  def time[F](f: => F, trials: Int) = {
    val t0 = System.nanoTime
    val ans = f
    printf("Elapsed: %.3f\n", 1e-9 * (System.nanoTime - t0) / trials)
    ans
  }

  def lots[F](n: Int, f: => F): F = if (n <= 1) f else {f; lots(n - 1, f)}

  def main(args: Array[String]) {
//
//    val size = 200000
//    val ri = RInt(Array.tabulate(size)(_.toInt))
//    ri.`attr<-`("dim", RInt(400, 500))
//
//    val in = (RInt(Array.tabulate(300)(_.toInt + 1)), RDouble(Array.tabulate(400)(_.toDouble + 1)))
//    val lin = List(in._1, in._2)
//
//    time(lots(10000, TwoDimensionalSubset(ri, 400, 500, in)), 10)
//    time(lots(10000, Subset.`[`(ri, lin)), 10)
//    val input = new PagedSeqReader(PagedSeq.fromFile("src/main/resources/test.r"))
//    println(input)


    val res = RParser.parseUnwrap(input) match {
      case e: Expression => println(e); Interpreter.interpret(e, RSession.currentSession.base)._1
      case _ => None
    }
    println(res)
  }

  val input =
  """
      f = function(...) {
        c("...")
      }
      f(1,2,3,4)
  """


}
