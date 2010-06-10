package uk.ac.ebi.sr
package interpreter

import collection.mutable.Map
import model.{Type, RObject}

/**
 *
 * Date: 30.05.2010
 * @author Taalai Djumabaev
 */

class Environment(identifiers: Map[String, Any], parent: Option[Environment]) extends RObject {
  lazy val child = new Environment(Map[String, Any](), Some(this))

  val ids = identifiers
  val objectType = Type.ENVIRONMENT

  def += (id: String, value: Any) = {
    ids += (id -> value)
    this
  }
  def ++= (vars: List[(String, Any)]) = {
    ids ++= vars
    this
  }

  def resolve(id: String): Option[Any] = {
    if (ids contains id) {
      Some(ids(id))
    } else {
      parent match {
        case Some(c) => c resolve id
        case None => None
      }
    }
  }

  override def toString() = {
    ids.toString
  }
}