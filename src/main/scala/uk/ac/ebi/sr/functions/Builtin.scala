package uk.ac.ebi.sr
package functions

import interpreter._
import model.RVal.{RInt, RChar, RBool, RDouble, RComplex}
import model._

/**
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
trait Assignable extends ArgMatching {
  def `<-`(args: List[FCallArg], fEnv: Environment, newValue: RObject): RObject = {
    //it is never empty list
    val name = args.head match {
      case CallArg(Var(i)) => i
      case CallArg(Lit(i)) => i
      case CallArgDef(n, Var(i)) => i
      case CallArgDef(n, Lit(i)) => i
      case NoneArg => error("invalid (NULL) left side of assignment")
      case _ => error("target of assignment expands to non-language object")
    }
    val newEnv = evalArgs(args, fEnv, fEnv)
    val res = assign(newEnv, newValue)
    fEnv.resolveWithEnv(name) match {
      case Some((o, e)) => if (o != res) {
        o.removeReferencer
        e += (name, res)
      }
      case None => //can't get here
    }
    if (!newEnv.isBound) newEnv.cleanAll
    res
  }

  protected def assign(env: Environment, newValue: RObject): RObject
}

abstract class Builtin extends RObject with ArgMatching with RFunction {
  import scala.collection.mutable.Map
  //todo should it be global environment. do we actually need this default one?
  lazy val defaultEvaluator = new Evaluator(Environment.emptyEnv)

  lazy val `type` = Type.BUILTIN

  def apply(args: List[FCallArg], fEnv: Environment) = {
    val newEnv = evalArgs(args, fEnv, fEnv)
    val res = apply(newEnv)
    if (!newEnv.isBound) newEnv.cleanAll
    res
  }

  protected def apply(env: Environment): RObject
}


case object Attr extends Builtin with Assignable {
  import AsInteger._
  import AsCharacter._
  import Length._

  val X = "x"
  val WHICH = "which"
  val EXACT = "exact"   // todo will be used when non-atomic vectors appear
  val DIM = "dim"
  val DIM_NAMES = "dimnames"
  val NAMES = "names"
  val params = List[FDeclArg](DeclArg(X), DeclArg(WHICH), DeclArgDef(EXACT, Num(RBool(0))))

  protected def apply(env: Environment): RObject = (env.resolve(X), env.resolve(WHICH)) match {
    case (Some(r), Some(n: RChar)) if (n.length == 1) => attr(r, n.s(0))
    case _ => NULL
  }

  protected def assign(env: Environment, newValue: RObject) =  (env.resolve(X), env.resolve(WHICH)) match {
    case (Some(r), Some(w: RChar)) if (w.length == 1) => `attr<-`(r, w.s(0), newValue)
    case _ => NULL
  }

  def attr(r: RObject, n: String) = r.attr(n)

  def `attr<-`(r: RObject, n: String, v: RObject) = {
    n match {
      case DIM => r match {
        case s: Sequential[_] => {
          val product = `as.integer`(v).s.foldLeft(1)((a, b) => a * b)
          if (product == s.length) setAttr(r, DIM, v)
          else error("product of dims: " + product + " did not match the object length: " + s.length)
        }
        case _ => error("wrong left part of the assignment")
      }
      //case DIM_NAMES =>
      case NAMES => r match {
        case s: Sequential[_] => {
          val names = `as.character`(v)
          if (names.length > s.length) error("'names' attribute must be the same length as the vector")
          else setAttr(s, NAMES, `length<-`(names, s.length))
        }
        case _ => setAttr(r, n, v)
      }
      case _ => setAttr(r, n, v)
    }
  }

  def setAttr(r: RObject, n: String, v: RObject) =
      if (r.isMultiReferenced) r.clone.asInstanceOf[RObject].`attr<-`(n, v) else r.`attr<-`(n, v)
}

case object Attributes extends Builtin {
  val OBJ = "obj"
  val params = List[FDeclArg](DeclArg(OBJ))

  protected def apply(env: Environment): RObject = env.resolve(OBJ) match {
    case Some(r) => this.attributes(r)
    case _ => NULL
  }

  def attributes(r: RObject) = {println(r.attributes); NULL }// todo should return a list of attributes
}  

case object Length extends Builtin with Assignable {
  import AsInteger._
  val X = "x"
  val params = List[FDeclArg](DeclArg(X))
  val zeroLength = RInt(0)

  protected def apply(env: Environment): RObject = env.resolve(X) match {
    case Some(r) => RInt(length(r))
    case _ => NULL
  }

  protected def assign(env: Environment, newValue: RObject) = {
    if (!newValue.isInstanceOf[RInt]) error("invalid value for length")
    env.resolve(X) match {
      case Some(r: Sequential[_]) => {
        val nv = `as.integer`(newValue)
        if (nv.length != 1) error("invalid argument for length ")
        val len = nv.s(0)
        if (len < 0) error("vector length cannot be negative")
        val res = `length<-`(r, len)(r.m)
        res
      }
      case Some(r) => error("invalid argument for length assignment")
      case _ => NULL
    }
  }

  def `length<-`[C](r: Sequential[C], nv: Int)(implicit m: Manifest[C]) = {
    if (nv == r.s.length) r
    else r.applyF({
      val newSeq = Array.tabulate[C](nv)(_ => r.NA)
      r.s.copyToArray(newSeq, 0, math.min(r.s.length, nv))
      newSeq
    })                     //todo no attrbute copying is done
  }

  def length(r: RObject) = r match {
    case s: Sequential[_] => s.length
    case _ => 0
  }
}

case object TypeOf extends Builtin {
  val X = "x"
  val params = List[FDeclArg](DeclArg(X))

  protected def apply(env: Environment): RObject = env.resolve(X) match {
    case Some(r) => typeof(r)
    case _ => NULL
  }

  def typeof(o: RObject) = RChar(o.`type`.toString)
}


case object Concat extends Builtin with Primitive {
  import Operations.concatArrays
  import AsInteger._
  import AsLogical._
  import AsDouble._
  import AsCharacter._

  val params = List[FDeclArg](ThreeLDots)

  protected def apply(env: Environment): RObject = evalLDots(env, concat)

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

case object RLangList {

  val params = List[FDeclArg]()

  
}

trait Primitive extends RObject {

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

}

// try, which, print, colnames, rownames, is.null, write.table, list, index, $, %in%, return,  diag(n), log2(x),
// new("AnnotatedDataFrame", data=efscv), require

