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

import reflect.Manifest
import functions.Attr
import model.RVal.{RChar, RBool}
import interpreter.NULL
import collection.mutable.ArrayBuffer

/**
 * Trait for recursive data structures
 *
 * Date: Jul 28, 2010
 * @author Taalai Djumabaev
 */
trait Recursive extends Sequential[RObject]

/**
 * list as it is in r language
 */
class RList(val s: Array[RObject]) extends Recursive {

  lazy val `type` = RList.`type`

  val NA = RBool()
  val m = RList.m

  def applyF(f: => Array[RObject]) = RList(f)

  /**
   * extract a value using 'names' attribute ('$' operation)
   */
  def extract(name: String): RObject = attributes.get(Attr.NAMES) match {
    case Some(c: RChar) => val res = searchForName(c.s, name); res
    case _ => NULL
  }

  protected def searchForName(a: Array[String], name: String): RObject = {
    val len = a.length
    val nLen = name.length
    var i = 0
    val found = new ArrayBuffer[Int]()

    while (i < len) {
      val value = a(i)
      if (value != null && value.startsWith(name)) {
        if (value.length == nLen) return this.s(i)
        else found += i
      }
      i += 1
    }
    if (found.size == 1) this.s(found.head) else NULL
  }

  override def toString = if (isEmpty) NULL.asString else s.toList.toString
}

object RList {

  val `type` = Type.LIST
  val m = Manifest.classType[RObject](classOf[RObject])

  def apply(s: Array[RObject]) = new RList(s)

  def apply(s: Array[RObject], n: Array[String]) = {
    val l = new RList(s)
    Attr.`attr<-`(l, Attr.NAMES, RChar(n))
    l
  }
}