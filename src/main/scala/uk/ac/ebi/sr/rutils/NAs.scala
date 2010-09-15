package uk.ac.ebi.sr
package rutils

import uk.ac.ebi.sr.model.Complex

/**
 * NA values for primitive types
 * 
 * Date: Jul 13, 2010
 * @author Taalai Djumabaev
 */
object NAs {
  val boolNA: Int = java.lang.Integer.MAX_VALUE
  val intNA: Int = java.lang.Integer.MAX_VALUE
  val doubleNA: Double = java.lang.Double.MAX_VALUE
  val complexNA: Complex = null.asInstanceOf[Complex]
  val charNA: String = null.asInstanceOf[String]
}