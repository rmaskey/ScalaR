package uk.ac.ebi.sr.model

import uk.ac.ebi.sr.interpreter.NULL

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */

abstract class RObject {

  val `type`:Type.Type

  lazy val attributes = collection.mutable.Map[String, RObject]()

  def attr(name: String): RObject = attributes.get(name) match {
    case Some(x) => x
    case None => NULL
  }

  def `attr<-`(name: String, value: RObject): RObject = {
    attributes += name -> value
    value
  }
}