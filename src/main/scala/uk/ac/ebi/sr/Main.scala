package uk.ac.ebi.sr

import functions.{TwoDimensionalSubset, Operations, Subset}
import interpreter.RLexer
import collection.mutable.ArrayBuffer
import model.{Type, RObject}
import model.RVal._

/**
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

  class Check[A] {

  }

  class CheckInt[Int] extends Check[Int]
  class CheckDouble[Double] extends Check[Double]

  def main(args: Array[String]) {

    val size = 200000
    val ri = RInt(Array.tabulate(size)(_.toInt))
    ri.`attr<-`("dim", RInt(400, 500))

    val in = (RInt(Array.tabulate(300)(_.toInt + 1)), RDouble(Array.tabulate(400)(_.toDouble + 1)))
    val lin = List(in._1, in._2)

    time(lots(10000, TwoDimensionalSubset(ri, 400, 500, in)), 10)
    time(lots(10000, Subset.`[`(ri, lin)), 10)
  }
}
