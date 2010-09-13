package uk.ac.ebi.sr
package model

import interpreter.NULL

/**
 * Counting references to the object.
 * If more than one present, further changes are applied to a clone
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

/**
 * Base class for all R objects.
 * `type` field is abstract (not ANY) so that subclasses are forced to override it
 */
abstract class RObject extends ReferenceCounter with Cloneable {

  /**
   * every object in R lang has it's type
   */
  val `type`:Type.Type

  /**
   * every object can have attributes which are also R objects
   */
  lazy val attributes = collection.mutable.Map[String, RObject]()

  /**
   * get an attribute
   */
  def attr(name: String): RObject = attributes.get(name) match {
    case Some(x) => x
    case None => NULL
  }

  /**
   * set an attribute. For internal use only. use functions.Attr.`attr<-`(...) method to set.
   */
  @deprecated
  def `attr<-`(name: String, value: RObject): RObject = {
    attributes += name -> value
    this
  }

  def dropAttr(names: String*) = for (name <- names) attributes.remove(name)

  override def clone() = super.clone
}