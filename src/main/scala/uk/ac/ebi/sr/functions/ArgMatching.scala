package uk.ac.ebi.sr
package functions

import interpreter._
import model.{Environment, RObject}

/**
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
trait ArgMatching {
  import scala.collection.mutable.ListBuffer

  def defaultEvaluator: Evaluator
  val params: List[FDeclArg]
  /**
   * Matches the tags and stores the mappings into provided map (env) and returns
   * all the unmatched declared arguments
   *
   * @param taggedArgs tagged arguments of the function call
   * @param declArgs declared arguments of the function
   * @param env keeps matched arguments
   * @param fEnv all other assingments during argument evaluation
   *
   * @return a pair of remaining list of unmatched declared arguments and arguments potentially for ... parameter
   */
  def matchTags(taggedArgs: List[CallArgDef], declArgs: List[FDeclArg], fEnv: Environment, env: Environment)
  : Pair[List[FDeclArg], List[CallArgDef]] = {
    val currParams = new ListBuffer[FDeclArg]() ++ declArgs
    val diffTaggedArgs  = new ListBuffer[CallArgDef]()
    taggedArgs foreach (a => declArgs filter (_.name == a.name) match {
      case found :: Nil =>
        env += (found.name, new Evaluator(fEnv) eval a.e)
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
            env += (found.name, new Evaluator(fEnv) eval arg.e)
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
   * @param fEnv keeps all other assignments during argument evaluation
   */
  def matchPos(unTagged: List[FCallArg], declArgs: List[FDeclArg], fEnv: Environment, env: Environment) {
    unTagged match {
      //todo bug is here if declargsdef present and nonearg
      case NoneArg :: Nil => if (declArgs.filter(!_.isInstanceOf[DeclArgDef]).size > 0) error("not enough arguments")
        else if (declArgs.isEmpty) return
      case _ =>
    }
    val (f, s) = declArgs span (_.name != "...")
    val unTaggedForProcess = if (s.isEmpty) unTagged else {
      val (a, b) = unTagged splitAt f.size
      env += ("...", LDotList(b))
      a
    }
    if (unTaggedForProcess.size > f.size) error(unTaggedForProcess +  " unused parameters present " + f) //todo toString method in Args
    (unTaggedForProcess zip f) foreach (arg => arg match {
      case (a: CallArg, h) => env += (h.name, new Evaluator(fEnv) eval a.e)
      case (a, h: DeclArgDef) if (a == NoneArg) => env += (h.name, defaultEvaluator eval h.default)
      case _ => // an error can come up when a variable is requested
    })
    // evaluate default parameters left  todo to check s (...)
    evalRemaining((f drop unTaggedForProcess.size) ::: s, env)
  }

  def evalRemaining(declArgs: List[FDeclArg], env: Environment) {
    declArgs foreach (_ match {
      case p: DeclArgDef => env += (p.name, defaultEvaluator eval p.default)
      case _ => // do nothing. An error can come up when a variable is requested but not found
    })
  }

  import scala.collection.mutable.Map

  def evalArgs(args: List[FCallArg], fEnv: Environment, enclosing: Environment) = {
    val env = new Environment(Map[String, RObject](), Some(enclosing))
    val tagged = args filter (_.isInstanceOf[CallArgDef])
    val (declArgs, remaining) = if (tagged.size > 0)
      matchTags(tagged.asInstanceOf[List[CallArgDef]], params, fEnv, env) else (params, List())
    val unTagged = args filter (a => !a.isInstanceOf[CallArgDef] || remaining.exists (_ == a))
    if (unTagged.size > 0) matchPos(unTagged, declArgs, fEnv, env) else evalRemaining(declArgs, env)
    env
  }
}