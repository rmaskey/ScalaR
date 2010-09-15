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

import collection.mutable.Map
import interpreter.{Evaluator, Expression}

/**
 * R environment. Has references on parent and children for a proper
 * clearing (since reference counters of objects contained need to be decreased during clean out)
 * 
 * Date: 30.05.2010
 * @author Taalai Djumabaev
 */
class Environment(val ids: Map[String, RObject] = Map[String, RObject](),
                  val parent: Option[Environment] = None) extends RObject {
  parent match {
    case Some(x: Environment) => x.children += this
    case None => // do nothing
  }
  lazy val children = collection.mutable.ListBuffer[Environment]()
  lazy val packageExports = Map[String, Map[String, RObject]]()

  /**
   * add a package into environment and search path.
   */
  def addPackage(name: String, exported: Map[String, RObject]) = {
    //todo to check names of variables (ome can be already present. warning is needed)
    packageExports += name -> exported
  }

  val `type` = Type.ENVIRONMENT
  var isBound = false

  /**
   * add new values to the environment. Expressions are evaluated before being added.
   */
  def ++= (m: Map[String, RObject]) = {
    for ((k, v) <- m) this += (k, v)
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

  /**
   * looks for id only in itself without looking in parent environments
   */
  //todo do we need to add exported variables for search? can we modify them?
  def resolveLocal(id: String): Option[RObject] = if (ids contains id) ids get id else None

  def resolve(id: String): Option[RObject] = {
    if (ids contains id) return ids get id
    packageExports.foreach((p : (String, Map[String, RObject])) => if (p._2 contains id) return p._2.get(id))
    parent match {
        case Some(c) => c.resolve(id)
        case None => None
    }
  }

  /**
   * resolve the id and returns object referenced with the containing environment
   */
  //todo do we need to add exported variables for search? can we modify them?
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

  def cleanAll() = if (cleanUp()) parent match {
    case Some(x: Environment) => x.children -= this
    case None => // do nothing
  }

  def cleanUp(): Boolean = {
    var allCleaned = true
    children.foreach(e => if (!e.isBound) e.cleanUp else allCleaned = false)
    if (allCleaned) {
      children.clear
      ids.foreach(_._2.removeReferencer)
      ids.clear
    }
    packageExports.foreach(_._2.clear)
    allCleaned
  }
}

object Environment {

  def emptyEnv = new Environment()

  def childEnv(parent: Environment) = new Environment(parent = Some(parent))
}