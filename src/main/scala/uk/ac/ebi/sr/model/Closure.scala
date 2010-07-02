package uk.ac.ebi.sr.model

import uk.ac.ebi.sr.interpreter._

/**
 *
 * Date: Jun 28, 2010
 * @author Taalai Djumabaev
 */

trait RFunction[T] extends ((List[T], Environment) => Any) {
  import scala.collection.mutable.Map
  import scala.collection.mutable.ListBuffer

//  def formals //not decided the type yet
//
//  def body: Expression
//
//  def environment: Environment
  /**
   * Matches the tags and stores the mappings into provided map (env) and returns
   * all the unmatched declared arguments
   *
   * @param taggedArgs tagged arguments of the function call
   * @param declArgs declared arguments of the function
   * @param env keeps matched arguments
   *
   * @return a pair of remaining list of unmatched declared arguments and arguments potentially for ... parameter
   */
  def matchTags(taggedArgs: List[CallArgDef], declArgs: List[FDeclArg], env: Environment)
  : Pair[List[FDeclArg], List[CallArgDef]] = {
    val currParams = new ListBuffer[FDeclArg]() ++ declArgs
    val diffTaggedArgs  = new ListBuffer[CallArgDef]()
    taggedArgs foreach (a => declArgs filter (_.name == a.name) match {
      case found :: Nil =>
        env += (found.name, a.e)
        currParams -= found
      case List() => diffTaggedArgs += a
      //can't get here since parser checks for same tags
      case _ => error("argument tag satisfies to more than one tags: " + a.name)
    })
    //partial matching. When a "..." parameter reached matching is stopped
    if (diffTaggedArgs.size > 0) {
      val forPartialMatching = currParams.toList.takeWhile(_.name != "...")
      val forLDot = new ListBuffer[CallArgDef]()
      diffTaggedArgs foreach (arg =>
        forPartialMatching filter (_.name.startsWith(arg.name)) match {
          case found :: Nil =>
            env += (found.name, arg.e)
            currParams -= found
          case List() => forLDot += arg//error("unknown parameter tag: " + arg.name + "   " + forPartialMatching)
          case _ => error("argument tag satisfies to more than one tags: " + arg.name)
        })
      return (currParams.toList, forLDot.toList)
    }
    (currParams.toList, List())
  }

  /**
   * @param unTagged arguments of the function call which are untagged
   * @param declArgs declared arguments of the function
   * @param env keeps matched arguments
   */
  def matchPos(unTagged: List[FCallArg], declArgs: List[FDeclArg], env: Environment) {
    unTagged match {
      case NoneArg :: Nil => if (declArgs.size > 0) error("not enough arguments") else return
      case _ =>
    }
    val (f, s) = declArgs span (_.name != "...")
    val unTaggedForProcess = if (s.isEmpty) unTagged else {
      val (a, b) = unTagged splitAt f.size
      env += ("...", b)
      a
    }
    if (unTaggedForProcess.size > f.size) error(unTaggedForProcess +  " unused parameters present " + f) //todo toString method in Args
    (unTaggedForProcess zip f) foreach (arg => arg match {
      case (a: CallArg, h) => env += (h.name, a.e) // todo to check - eval(a.e) needed?
      case (a, h: DeclArgDef) if (a == NoneArg) => env += (h.name, h.default)
      case _ => // an error can come up when a variable is requested
    })

    // evaluate default parameters left
    (f drop unTaggedForProcess.size) ::: s foreach (_ match {
      case p: DeclArgDef => env += (p.name, p.default)
      case _ => // do nothing. An error can come up when a variable is requested but not found
    })
  }
}

case class Closure(params: List[FDeclArg], expr: Expression, env: Environment)
extends RObject with RFunction[FCallArg] {
  lazy val `type` = Type.CLOSURE
  import scala.collection.mutable.Map

  def formals = params

  def body = expr

  def environment = env

  def apply(args: List[FCallArg], fEnv: Environment) = {
    val environment = new Environment(Map[String, Any](), Some(fEnv))
    val tagged = args filter (_.isInstanceOf[CallArgDef])
    val (declArgs, remaining) = if (tagged.size > 0)
      matchTags(tagged.asInstanceOf[List[CallArgDef]], params, environment) else (params, List())
    val unTagged = args filter (a => !a.isInstanceOf[CallArgDef] || remaining.exists (_ == a))
    if (unTagged.size > 0) matchPos(unTagged, declArgs, environment)
    Evaluator.eval(expr, environment)
  }

  override def toString() = "Closure"
}

abstract class Builtin extends RObject with RFunction[FCallArg] {
  import scala.collection.mutable.Map
  lazy val `type` = Type.BUILTIN
  val params: List[FDeclArg]

  def apply(args: List[FCallArg], fEnv: Environment) = {
    val environment = new Environment(Map[String, Any](), Some(fEnv))
    val tagged = args filter (_.isInstanceOf[CallArgDef])
    val (declArgs, remaining) = if (tagged.size > 0)
      matchTags(tagged.asInstanceOf[List[CallArgDef]], params, environment) else (params, List())
    val unTagged = args filter (a => !a.isInstanceOf[CallArgDef] || remaining.exists (_ == a))
    if (unTagged.size > 0) matchPos(unTagged, declArgs, environment)
    eval(environment)
  }

  protected def eval(env: Environment): Any

}

import RVal._
object attr extends Builtin {
  val params = List[FDeclArg](new DeclArg("x"), new DeclArg("which"), new DeclArgDef("exact", new Num(RBool(0))))

  import scala.collection.mutable.Map
  protected def eval(env: Environment): Any = {
    (env.resolve("x"), env.resolve("which")) match {
      case (Some(r: RObject), Some(w: String)) => r.getAttr(w)
      case _ => NULL
    }
  }
}

object `attr<-` extends Builtin {
  val params = List[FDeclArg](new DeclArg("x"), new DeclArg("which"), new DeclArg("nv"))

  import scala.collection.mutable.Map
  protected def eval(env: Environment): Any = {
    (env.resolve("x"), env.resolve("which"), env.resolve("nv")) match {
      case (Some(r: RObject), Some(w: String), Some(n)) => r.addAttr(w, n); n
      case _ => NULL
    }
  }
}


class Built {

}

case object Println extends RFunction[Any] {
  import collection.mutable.LinkedHashSet
  val printLines = LinkedHashSet[String]()

  def formals = null

  def body = null

  def environment = null

  def getAndClearLines = {
    val tmp = printLines.mkString("\n")
    printLines.clear
  }

  def apply(args: List[Any], e: Environment) = args match {
    case (v: String) :: Nil => {println("\"" + v + "\""); v}
    case v :: Nil => {println(v); printLines += v.toString; v}
    case _ => error("println needs 1 argument")
  }
}























