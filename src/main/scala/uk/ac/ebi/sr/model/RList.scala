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