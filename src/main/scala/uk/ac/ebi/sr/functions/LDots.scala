package uk.ac.ebi.sr
package functions

import interpreter._
import model.{RList, Type, RObject, Environment}
import model.RVal._

/**
 *
 * Date: Sep 10, 2010
 * @author Taalai Djumabaev
 */
trait LDots {

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

case object Concat extends Builtin with LDots {
  import Operations.concatArrays
  import AsInteger._
  import AsLogical._
  import AsDouble._
  import AsCharacter._

  val params = List[FDeclArg](ThreeLDots)

  protected def process(env: Environment): RObject = evalLDots(env, concat)

  def concat(obs: Seq[RObject]): RObject = {
    if (obs.size == 0) return NULL
    if (obs.size == 1) return obs.head
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

case object RLangList extends Builtin with LDots {

  val params = List[FDeclArg](ThreeLDots)

  protected def process(env: Environment): RObject = evalLDotsWithNames(env, list)

  def list(obs: Seq[RObject], names: Seq[String]): RObject = RList(obs.toArray, names.toArray)
}