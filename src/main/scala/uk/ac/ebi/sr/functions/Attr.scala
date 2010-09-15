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
package functions

import interpreter._
import model.{RList, Sequential, Environment, RObject}
import model.RVal.{RChar, RBool}

/**
 * Function for getting and setting (through Assignable trait) attributes to the objects
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
case object Attr extends Assignable {
  import Length._

  val X = "x"
  val WHICH = "which"
  val EXACT = "exact"   // todo will be used when non-atomic vectors appear
  val DIM = "dim"
  val DIM_NAMES = "dimnames"
  val NAMES = "names"
  val params = List[FDeclArg](DeclArg(X), DeclArg(WHICH), DeclArgDef(EXACT, Num(RBool(0))))

  protected def process(env: Environment): RObject = (env.resolve(X), env.resolve(WHICH)) match {
    case (Some(r), Some(n: RChar)) if (n.length == 1) => apply(r, n.s(0))
    case _ => NULL
  }

  protected def assign(env: Environment, newValue: RObject) =  (env.resolve(X), env.resolve(WHICH)) match {
    case (Some(r), Some(w: RChar)) if (w.length == 1) => `attr<-`(r, w.s(0), newValue)
    case _ => NULL
  }

  /**
   * getting requiered attribute from the RObject
   */
  def apply(r: RObject, n: String) = r.attr(n)

  /**
   * setting a new value for the attribute to the RObject 
   */
  def `attr<-`(r: RObject, n: String, v: RObject) = n match {
    case DIM => r match {
      case s: Sequential[_] => {
        val product = AsInteger(v).s.foldLeft(1)((a, b) => a * b)
        if (product == s.length) setAttr(r, DIM, v)
        else error("product of dims: " + product + " did not match the object length: " + s.length)
      }
      case _ => error("wrong left part of the assignment")
    }
    //todo case DIM_NAMES =>

    case NAMES => r match {
      case s: Sequential[_] => {
        val names = AsCharacter(v)
        if (names.length > s.length) error("'names' attribute must be the same length as the vector")
        else setAttr(s, NAMES, `length<-`(names, s.length))
      }
      case _ => setAttr(r, n, v)
    }
    case _ => setAttr(r, n, v)
  }

  def setAttr(r: RObject, n: String, v: RObject) =
      if (r.isMultiReferenced) r.clone.asInstanceOf[RObject].`attr<-`(n, v) else r.`attr<-`(n, v)
}

/**
 * Function for getting all attributes as a RList.
 * Names of the attributes form the NAMES attribute
 * in the resulting list
 */
case object Attributes extends StdBuiltin("obj") {

  def apply(r: RObject) = {
    val a = r.attributes
    val names = new Array[String](a.size)
    val objs = new Array[RObject](a.size)

    var i = 0
    a.toList.foreach((p: (String, RObject)) => {
      names(i) = p._1
      objs(i) = p._2
      i += 1
    })
    RList(objs, names)
  }
}