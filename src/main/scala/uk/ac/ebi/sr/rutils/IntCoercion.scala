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
package rutils

import model.{Complex}
import model.RVal.Bool
import NAs._

/**
 * Primitive type coercions
 *
 * Date: Jul 13, 2010
 * @author Taalai Djumabaev
 */
object BoolCoercion {

  def char2Bool(s: String): Bool = if (s == charNA) boolNA // needed since NA is just null
    else if (s == "TRUE"  || s == "true") 1
    else if (s == "FALSE" || s == "false") 0
    else boolNA

  def int2Bool(i: Int) = i match {
    case 0 => 0
    case `intNA` => i
    case any => 1
  }

  def double2Bool(d: Double): Bool = if (d == doubleNA) boolNA else d.toInt

  def complex2Bool(c: Complex): Bool = {
    if (c == complexNA) return boolNA
    if (c.i != 0) {}//todo warning
    c.r.toInt
  }
}

object IntCoercion {

  def bool2Int(b: Bool) = b

  def double2Int(d: Double): Int = if (d == doubleNA) intNA else d.toInt

  def complex2Int(c: Complex): Int = {
    if (c == complexNA) return intNA
    if (c.i != 0) {}//todo warning
    c.r.toInt
  }

  def char2Int(s: String): Int = {
    if (s == charNA) intNA
    else try {
      java.lang.Double.valueOf(s).doubleValue.toInt
    } catch { case e: NumberFormatException => intNA }
  }
}

object DoubleCoercion {

  def bool2Double(i: Bool): Double = if (i == boolNA) doubleNA else i.toDouble
  def int2Double(i: Int): Double = if (i == intNA) doubleNA else i.toDouble

  def complex2Double(c: Complex): Double = {
    if (c == complexNA) return doubleNA
    if (c.i != 0) {}// warning
    c.r
  }

  def char2Double(s: String): Double = {
    if (s == charNA) doubleNA
    try {
      java.lang.Double.valueOf(s).doubleValue
    } catch { case e: NumberFormatException => doubleNA }
  }
}

object ComplexCoercion {

  def bool2Complex(b: Bool): Complex = if (b == boolNA) complexNA else new Complex(b)
  def int2Complex(i: Int): Complex = if (i == intNA) complexNA else new Complex(i)
  def double2Complex(d: Double): Complex = if (d == doubleNA) complexNA else new Complex(d)

  def char2Complex(s: String): Complex = {
    //todo parsing needed
    complexNA
  }
}

object CharCoercion {

  def bool2Char(b: Bool) = b match {
    case 1 => "TRUE"
    case 0 => "FALSE"
    case `boolNA` => charNA
    case a => error("internal error: undefined value for boolean " + a) 
  }

  def int2Char(i: Int): String = if (i == intNA) charNA else i.toString

  def double2Char(d: Double): String = if (d == doubleNA) charNA else d.toString

  def complex2Char(c: Complex): String = if (c == complexNA) charNA else c.toString
}
