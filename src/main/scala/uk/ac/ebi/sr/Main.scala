package uk.ac.ebi.sr

import interpreter.RLexer
import collection.mutable.ArrayBuffer
import model.{Type, RObject}
import functions.Operations

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

  def adSum(ad: Array[Double]) = {
    var sum = 0.0
    var i = 0
    while (i < ad.length) {sum += ad(i); i += 1}
    sum
  }

  def addTwo(a: Array[Double], d: Array[Double]) = {
    var i = 0
    val buf = new ArrayBuffer[Double](a.length)
    while (i < a.length) {
      buf += a(i) + d(i)
      i+=1
    }
    buf.toArray
  }

  class A
  class B extends A {
    def b() = 0
  }

  def main(args: Array[String]) {
    val size = 1000000
    val trials = 1
    val i = Integer.MAX_VALUE
    println(java.lang.Double.parseDouble("111111e2222222"))
    val b = List[A](new B())
    b.asInstanceOf[List[B]].head.b
    println(-2.5.toInt )
//    val a = Array.tabulate(size)(_.toDouble)
//    val d = Array.tabulate(size)(_.toDouble)
//    val ab = new collection.mutable.ArrayBuffer[Double] ++ a
//    import model.RVal._
//    val ra = RDouble(a)
//    val rd = RDouble(d)
//    time(lots(trials, addTwo(a,d)), trials)
//    time(lots(trials, Operations.sum(ra,rd)), trials)
//
//
//    time(lots(trials, ab.sum), trials)
//    time(lots(trials, (0.0 /: ab)(_ + _)), trials)
//    time(lots(trials, (0.0 /: a)(_ + _)), trials)
//    time(lots(trials, {var s = 0.0; ab.foreach(s += _); s}), trials)
//    time(lots(trials, adSum(a)), trials)
  }
}
