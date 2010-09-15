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
import model.{RList, Type, RObject, Environment}
import model.RVal._

/**
 * trait is mixed when support for variable number of arguments is needed
 * in ArgMatching trait ldot argument is a list of unevaluated Expressions
 *
 * Date: Sep 10, 2010
 * @author Taalai Djumabaev
 */
trait LDots {

  /**
   * evaluates the list of unevaluated expressions
   */
  def evalLDots(env: Environment, f: Seq[RObject] => RObject) = env.resolve(ThreeLDots.name) match {
    case Some(LDotList(l)) => {
      val evaluator = new Evaluator(env)
      f(for (fa <- l) yield fa match {
        case CallArg(e) => evaluator.eval(e)
        case CallArgDef(n, e) => {
          val res = evaluator.eval(e)
          env += (n, res)
          res
        }
        case _ => error("internal error: wrong arguments for funcall ")
      })
    }
    case _ => NULL
  }

  /**
   * evaluates the list of unevaluated expressions. Tagged expressions are put into
   * `names` attribute
   */
  def evalLDotsWithNames(env: Environment, f : (Seq[RObject], Seq[String]) => RObject) =
  env.resolve(ThreeLDots.name) match {
    case Some(LDotList(l)) => {
      val evaluator = new Evaluator(env)
      val names = new Array[String](l.size)
      var i = -1
      val list = for (fa <- l) yield {
        i += 1
        fa match {
          case CallArg(e) => evaluator.eval(e)
          case CallArgDef(n, e) => {
            val res = evaluator.eval(e)
            env += (n, res)
            names(i) = n
            res
          }
          case _ => error("internal error: wrong arguments for funcall ")
        }
      }
      f(list, names)
    }
    case _ => NULL
  }
}

/**
 * function that concatenates variable number of arguments into an RObject
 * of the most abstract type possible
 */
case object Concat extends Builtin with LDots {
  import Operations.concatArrays
  import AsInteger._
  import AsLogical._
  import AsDouble._
  import AsCharacter._

  //one `ldots` parameter
  val params = List[FDeclArg](ThreeLDots)

  protected def process(env: Environment): RObject = evalLDots(env, concat)

  def concat(obs: Seq[RObject]): RObject = {
    if (obs.size == 0) return NULL
    if (obs.size == 1) return obs.head
    //find the most abstract type possible
    var value = 0
    for (o <- obs) {
      val res = Type.typeValue(o.`type`)
      if (value < res) value = res
    }
    var totalLength = 0
    value match {
      case 1 => concatArrays(obs, `as.logical`)(RBool.m)
      case 2 => concatArrays(obs, `as.integer`)(RInt.m)
      case 3 => concatArrays(obs, `as.double`)(RDouble.m)
      case 4 =>  NULL  //todo complex
      case 5 => concatArrays(obs, `as.character`)(RChar.m)
      case _ => RList(obs.toArray)
    }
  }
}

/**
 * function that creates list from variable number of arguments
 */
case object RLangList extends Builtin with LDots {

  val params = List[FDeclArg](ThreeLDots)

  protected def process(env: Environment): RObject = evalLDotsWithNames(env, list)

  def list(obs: Seq[RObject], names: Seq[String]): RObject = RList(obs.toArray, names.toArray)
}