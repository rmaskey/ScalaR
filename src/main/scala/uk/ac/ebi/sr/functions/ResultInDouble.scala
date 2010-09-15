package uk.ac.ebi.sr
package functions

import rutils.NAs
import model.RVal.{RDouble, RInt, RChar, RBool}
import model.Sequential
import Operations.convertArray

/**
 * An alternative to the Sequential's applyF method where
 * the resulting object's type is same to the calling object type
 *
 * Date: 09-Sep-2010
 * @author Taalai Djumabaev
 */
trait ResultInDouble {

  def applyForDouble[A](seq: Sequential[A], f: A => Double) =
    RDouble(convertArray(seq.s, f, seq.NA, NAs.doubleNA))
}

trait ResultInInt {

  def applyForInt[A](seq: Sequential[A], f: A => Int) =
    RInt(convertArray(seq.s, f, seq.NA, NAs.intNA))
}

trait ResultInBool {

  def applyForBool[A](seq: Sequential[A], f: A => Int) =
    RBool(convertArray(seq.s, f, seq.NA, NAs.boolNA))
}

trait ResultInChar {

  def applyForChar[A](seq: Sequential[A], f: A => String) =
    RChar(convertArray(seq.s, f, seq.NA, NAs.charNA))
}