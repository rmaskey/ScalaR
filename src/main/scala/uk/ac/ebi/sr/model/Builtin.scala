package uk.ac.ebi.sr
package model

import interpreter._
import RVal._

/**
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
trait Assignable {
  def `<-`(args: List[FCallArg], fEnv: Environment, newValue: Any): Any
}

abstract class Builtin extends RObject with ArgMatching with RFunction[FCallArg] {
  import scala.collection.mutable.Map

  lazy val `type` = Type.BUILTIN

  def apply(args: List[FCallArg], fEnv: Environment) = {
    evalArgs(args, fEnv)
    eval(new Environment(Map[String, Any](), Some(fEnv)))
  }

  protected def eval(env: Environment): Any
}


case object Attr extends Builtin with Assignable {
  val params = List[FDeclArg](new DeclArg("x"), new DeclArg("which"), new DeclArgDef("exact", new Num(RBool(0))))

  protected def eval(env: Environment): Any = {
    (env.resolve("x"), env.resolve("which")) match {
      case (Some(r: RObject), Some(w: String)) => r.getAttr(w)
      case _ => NULL
    }
  }

  def `<-`(args: List[FCallArg], fEnv: Environment, newValue: Any) = {
    evalArgs(args, fEnv)
    (fEnv.resolve("x"), fEnv.resolve("which")) match {
      case (Some(r: RObject), Some(w: RChar)) if (w.length == 1) => r.addAttr(w.s(0), newValue)
      case _ => println("x="+fEnv.resolve("x")); println(fEnv.resolve("which"));NULL
    }
  }
}

case object Attributes extends Builtin {
  val params = List[FDeclArg](new DeclArg("obj"))

  protected def eval(env: Environment): Any = {
    env.resolve("obj") match {
      case Some(r: RObject) => r.attributes
      case _ => NULL
    }
  }
}

case object Length extends Builtin with Assignable {
  val params = List[FDeclArg](new DeclArg("x"))

  protected def eval(env: Environment): Any = {
    env.resolve("x") match {
      case Some(r: RObject) => r.length
      case _ => NULL
    }
  }

  def `<-`(args: List[FCallArg], fEnv: Environment, newValue: Any) = {
    if (!newValue.isInstanceOf[RInt]) error("invalid value for length")
    evalArgs(args, fEnv)
    fEnv.resolve("x") match {
      case Some(r: Sequential[Any]) => {
        val nv = newValue.asInstanceOf[RInt]
        if (nv.length != 1) error("invalid argument for length ")
        fEnv += ("x", r.resize(nv.s(0)))
      }
      case Some(r: Object) => error("invalid argument for length assignment")
      case _ => NULL
    }
  }
}


//case object index extends Builtin with Assignable {
//
//}

// try, which, print, colnames, rownames, is.null, write.table, list, index, $, %in%, return,  diag(n), log2(x),
// new("AnnotatedDataFrame", data=efscv), require

