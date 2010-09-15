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
  import RObject._

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

  override def toString: String = {
    val buf = new StringBuilder
    attributes.foreach((a: (String, RObject)) => {
      buf append "\n"
      buf append attrToString(a._1)
      buf append "\n"
      buf append a._2.toString
    })
    buf toString
  }
}

object RObject {
  def attrToString(n: String) = "attr(" + n + ")"
}