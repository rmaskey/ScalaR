package uk.ac.ebi.sr.model

import uk.ac.ebi.sr.interpreter.NULL

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */
trait ReferenceCounter {
  var counter = 0

  def addReferencer() = counter += 1
  def removeReferencer() = counter -= 1

  def isMultiReferenced = counter > 1
  def isNotReferenced = counter <= 0
}

abstract class RObject extends ReferenceCounter with Cloneable {

  val `type`:Type.Type

  lazy val attributes = collection.mutable.Map[String, RObject]()

  def attr(name: String): RObject = attributes.get(name) match {
    case Some(x) => x
    case None => NULL
  }

  def `attr<-`(name: String, value: RObject): RObject = {
    attributes += name -> value
    this
  }

  override def clone() = super.clone
}