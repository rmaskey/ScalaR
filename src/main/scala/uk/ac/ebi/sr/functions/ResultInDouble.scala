package uk.ac.ebi.sr
package functions

import rutils.NAs
import model.RVal.RDouble

/**
 *
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */
trait ResultInDouble {

  def applyF[A](a: Array[A], f: A => Double) = {
    val length = a.length
    val newSeq = Array.tabulate[Double](length)(_ => NAs.doubleNA)
    var i = 0
    while (i < length) {
      a(i) match {
        case NAs.intNA =>
        case any => newSeq(i) = f(any)
      }
      i += 1
    }
    RDouble(newSeq)
  }
}