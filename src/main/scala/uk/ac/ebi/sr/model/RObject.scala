package uk.ac.ebi.sr.model

import uk.ac.ebi.sr.interpreter.NULL

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */

abstract class RObject {

  val `type`:Type.Type

  lazy val attributes = collection.mutable.Map[String, Any]()

  def attr(name: String) = attributes.get(name) match {
    case Some(x) => x
    case None => NULL
  }

  def `attr<-`(name: String, value: Any) = {
    attributes += name -> value
    value
  }
}