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
    assign(evalArgs(args, fEnv, fEnv), newValue)
    newValue
  }

  protected def assign(env: Environment, newValue: RObject)
}

abstract class Builtin extends RObject with ArgMatching with RFunction {
  import scala.collection.mutable.Map
  //todo should it be global environment. do we actually need this default one?
  lazy val defaultEvaluator = new Evaluator(new Environment(Map(), None))

  lazy val `type` = Type.BUILTIN

  def apply(args: List[FCallArg], fEnv: Environment) = apply(evalArgs(args, fEnv, fEnv))

  protected def apply(env: Environment): RObject
}


case object Attr extends Builtin with Assignable {
  val X = "x"
  val WHICH = "which"
  val EXACT = "exact"
  val DIM = "dim"
  val params = List[FDeclArg](new DeclArg(X), new DeclArg(WHICH), new DeclArgDef(EXACT, new Num(RBool(0))))

  protected def apply(env: Environment): RObject = {
    (env.resolve(X), env.resolve(WHICH)) match {
      case (Some(r), Some(n: RChar)) if (n.length == 1) => attr(r, n.s(0))
      case _ => NULL
    }
  }

  protected def assign(env: Environment, newValue: RObject) = {
    (env.resolve(X), env.resolve(WHICH)) match {
      case (Some(r), Some(w: RChar)) if (w.length == 1) => `attr<-`(r, w.s(0), newValue)
      case _ => NULL
    }
  }

  def attr(r: RObject, n: String) = r.attr(n)

  def `attr<-`(r: RObject, n: String, v: RObject) = {
    n match {
//      case DIM => r match {   TODO length should be checked
//        case s: Sequential[Any] => s
//      }
      case _ => r.`attr<-`(n, v)
    }
  }
}

case object Attributes extends Builtin {
  val OBJ = "obj"
  val params = List[FDeclArg](new DeclArg(OBJ))

  protected def apply(env: Environment): RObject = {
    env.resolve(OBJ) match {
      case Some(r) => println(r.attributes); NULL ///this.attributes(r)
      case _ => NULL
    }
  }

  def attributes(r: RObject) = println(r.attributes); NULL // todo should return a list of attributes
}  

case object Length extends Builtin with Assignable {
  val X = "x"
  val params = List[FDeclArg](new DeclArg(X))
  val zeroLength = RInt(0)

  protected def apply(env: Environment): RObject = {
    env.resolve(X) match {
      case Some(r) => RInt(length(r))
      case _ => NULL
    }
  }

  protected def assign(env: Environment, newValue: RObject) = {
    if (!newValue.isInstanceOf[RInt]) error("invalid value for length")
    env.resolve(X) match {
      case Some(r: Sequential[Any]) => {
        //todo a global coersion method toRInt
        val nv = newValue.asInstanceOf[RInt]
        if (nv.length != 1) error("invalid argument for length ")
        //previous value of 'x' will be lost
        env += (X, `length<-`(r, nv.s(0)))
      }
      case Some(r) => error("invalid argument for length assignment")
      case _ => NULL
    }
  }

  def `length<-`[C](r: Sequential[C], nv: Int) = r.resize(nv)

  def length(r: RObject) = r match {
    case s: Sequential[Any] => s.length
    case _ => 0
  }
}

case object AsLogical extends Builtin {
  import Operations.convertArray
  val X = "x"
  val params = List[FDeclArg](new DeclArg(X))

  protected def apply(env: Environment): RObject = {
    env.resolve(X) match {
      //todo to changde Any to RObject since string is RChar
      case Some(r) => `as.logical`(r)
      case _ => NULL
    }
  }

  def `as.logical`(x: Any): RBool = x match {
    case b: RBool => b
    case i: RInt => if (i.isEmpty) RBool() else new RBool(i.s)
    case d: RDouble => if (d.isEmpty) RBool() else new RBool(convertArray(d.s, (e :Double) => e.toInt))
    case c: RComplex => if (c.isEmpty) RBool() else new RBool(convertArray(c.s, (e: Complex) => if (e.isZero) 0 else 1))
    case c: RChar => if (c.isEmpty) RBool() else new RBool(convertArray(c.s, (e: String) =>
      if (e == "TRUE" || e == "true") 1
      else if (e == "FALSE" || e == "false") 0
      else error("argument is not interpretable as logical")))
    case _ => error("argument is not interpretable as logical")
  }
}


// try, which, print, colnames, rownames, is.null, write.table, list, index, $, %in%, return,  diag(n), log2(x),
// new("AnnotatedDataFrame", data=efscv), require

