package uk.ac.ebi.sr
package model

import collection.mutable.Map
import interpreter.{Evaluator, Expression}

/**
 *
 * Date: 30.05.2010
 * @author Taalai Djumabaev
 */

class Environment(val ids: Map[String, Any], parent: Option[Environment]) extends RObject {
  lazy val child = new Environment(Map[String, Any](), Some(this))

  val `type` = Type.ENVIRONMENT

  def += (id: String, value: Any) = {
    val v = value match {
      case e: Expression => Evaluator.eval(e, this)
      case any => any
    }
    ids += (id -> v)
    this
  }

  def resolve(id: String): Option[Any] = {
    if (ids contains id) {
      Some((ids(id), this))
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