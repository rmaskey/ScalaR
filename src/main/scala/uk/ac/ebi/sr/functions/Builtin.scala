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
  lazy val defaultEvaluator = new Evaluator(new Environment(Map(), None))

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

  val X = "x"
  val WHICH = "which"
  val EXACT = "exact"   // will be used when non-atomic vectors appear
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
        case s: Sequential[Any] => {
          val product = `as.integer`(v).s.foldLeft(1)((a, b) => a * b)
          if (product == s.length) {
            if (r.isMultiReferenced) r.clone.asInstanceOf[RObject].`attr<-`(n, v) else r.`attr<-`(n, v)
          } else error("product of dims: " + product + " did not match the object length: " + s.length)
        }
        case _ => error("wrong left part of the assignment")
      }
      //case DIM_NAMES =>
      //case NAMES =>
      case _ => if (r.isMultiReferenced) r.clone.asInstanceOf[RObject].`attr<-`(n, v) else r.`attr<-`(n, v)
    }
  }
}

case object Attributes extends Builtin {
  val OBJ = "obj"
  val params = List[FDeclArg](DeclArg(OBJ))

  protected def apply(env: Environment): RObject = env.resolve(OBJ) match {
    case Some(r) => println(r.attributes); NULL ///this.attributes(r)
    case _ => NULL
  }

  def attributes(r: RObject) = println(r.attributes); NULL // todo should return a list of attributes
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
      case Some(r: Sequential[Any]) => {
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
    })
  }

  def length(r: RObject) = r match {
    case s: Sequential[Any] => s.length
    case _ => 0
  }
}

case object AsLogical extends Builtin {
  import Operations.convertArray
  import rutils.BoolCoercion._
  val X = "x"
  val params = List[FDeclArg](DeclArg(X))

  protected def apply(env: Environment): RObject = {
    env.resolve(X) match {
      case Some(r) => `as.logical`(r)
      case _ => NULL
    }
  }

  def `as.logical`(x: RObject): RBool = x match {
    case b: RBool => b
    case i: RInt => RBool(i.s) // unless we change the bool type
    case d: RDouble => RBool(convertArray(d.s, (e: Double) => double2Bool(e)))
    case c: RComplex => RBool(convertArray(c.s, (e: Complex) => complex2Bool(e)))
    case c: RChar => RBool(convertArray(c.s, (e: String) => char2Bool(e)))//in if - error("argument is not interpretable as logical")))
    case _ => error("argument of type " + x.`type` + " is not interpretable as logical vector")
  }
}

case object AsInteger extends Builtin {
  import Operations.convertArray
  import rutils.IntCoercion._
  val X = "x"
  val params = List[FDeclArg](DeclArg(X))

  protected def apply(env: Environment): RObject = env.resolve(X) match {
    case Some(r) => `as.integer`(r)
    case _ => NULL
  }

  def `as.integer`(x: RObject): RInt = x match {
    case b: RBool => RInt(b.s) // unless we change the bool type
    case i: RInt => i
    case d: RDouble => RInt(convertArray(d.s, (e: Double) => double2Int(e)))
    case c: RComplex => RInt(convertArray(c.s, (e: Complex) => complex2Int(e)))
    case c: RChar => RInt(convertArray(c.s, (e: String) => char2Int(e)))
    case _ => error("argument of type " + x.`type` + " is not interpretable as integer vector")
  }

}


// try, which, print, colnames, rownames, is.null, write.table, list, index, $, %in%, return,  diag(n), log2(x),
// new("AnnotatedDataFrame", data=efscv), require

