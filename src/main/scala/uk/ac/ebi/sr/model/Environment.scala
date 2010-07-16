package uk.ac.ebi.sr
package model

import collection.mutable.Map
import interpreter.{Evaluator, Expression}

/**
 *
 * Date: 30.05.2010
 * @author Taalai Djumabaev
 */

class Environment(val ids: Map[String, RObject], parent: Option[Environment]) extends RObject {
  parent match {
    case Some(x: Environment) => x.children += this
    case None => // do nothing
  }
  lazy val children = collection.mutable.ListBuffer[Environment]()

  val `type` = Type.ENVIRONMENT
  var isBound = false

  def ++= (l: List[(String, RObject)]) = {
    for ((k, v) <- l) this += (k, v)
    this
  }

  def += (id: String, value: RObject) = {
    val v: RObject = value match {
      case e: Expression => Evaluator.eval(e, this)
      case _ => value
    }
    ids += (id -> v)
    v.addReferencer
    this
  }

  def resolveLocal(id: String): Option[RObject] = if (ids contains id) ids get id else None

  def resolve(id: String): Option[RObject] = {
    if (ids contains id) ids get id
    else parent match {
        case Some(c) => c.resolve(id)
        case None => None
    }
  }

  def resolveWithEnv(id: String): Option[(RObject, Environment)] = {
    if (ids contains id) Some((ids(id), this))
    else parent match {
        case Some(c) => c.resolveWithEnv(id)
        case None => None
    }
  }

  override def toString() = {
    ids.toString
  }

  def cleanAll() {
    cleanUp()
    parent match {
      case Some(x: Environment) => x.children -= this
      case None => // do nothing
    }
  }

  def cleanUp() {
    children.foreach(_.cleanUp)
    ids.foreach(_._2.removeReferencer)
    ids.clear
  }
}