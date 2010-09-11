package uk.ac.ebi.sr
package functions

import interpreter._
import model.{Environment, RObject}

/**
 * A trait used to match arguments of a calling function to it's declared arguments.
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
trait ArgMatching {
  import scala.collection.mutable.ListBuffer

  /*
   * default values of the arguments are evaluated in separate default evaluator 
   */
  def defaultEvaluator: Evaluator

  /*
   * list of declared arguments
   */
  val params: List[FDeclArg]

  /**
   * Method used to match arguments of a calling function to it's declared arguments.
   *
   * @param args arguments function being called with
   * @param fEnv calling environment of function
   * @param enclosing environment of function
   */
  def evalArgs(args: List[FCallArg], fEnv: Environment, enclosing: Environment) = {
    val env = Environment.childEnv(enclosing)
    val tagged = args filter (_.isInstanceOf[CallArgDef])
    val (declArgs, remaining) = if (tagged.size > 0)
      matchTags(tagged.asInstanceOf[List[CallArgDef]], params, fEnv, env) else (params, List())
    val unTagged = args filter (a => !a.isInstanceOf[CallArgDef] || remaining.exists (_ == a))
    if (unTagged.size > 0) matchPos(unTagged, declArgs, fEnv, env) else evalRemaining(declArgs, env)
    env
  }

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
  private def matchTags(taggedArgs: List[CallArgDef], declArgs: List[FDeclArg], fEnv: Environment, env: Environment)
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
      val forPartialMatching = currParams.toList.takeWhile(_.name != ThreeLDots.name)
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
  private def matchPos(unTagged: List[FCallArg], declArgs: List[FDeclArg], fEnv: Environment, env: Environment) {
    unTagged match {
      case NoneArg :: Nil => if (!declArgs.contains(ThreeLDots)) {
        if (declArgs.filter(!_.isInstanceOf[DeclArgDef]).size > 0) error("not enough arguments")
        else if (!declArgs.isEmpty) return evalRemaining(declArgs, env)
        else return
      }
      case _ =>
    }
    val (f, s) = declArgs span (_.name != ThreeLDots.name)
    val unTaggedForProcess = if (s.isEmpty) unTagged else {
      val (a, b) = unTagged splitAt f.size
      val callArgs = new ListBuffer[FCallArg]
      for (arg <- b) arg match {
        case CallArg(ThreeLDots) => fEnv.resolve(ThreeLDots.name) match {
          case Some(LDotList(l : List[FCallArg])) => callArgs ++= l
          case _ => callArgs += arg
        }
        case _ => callArgs += arg
      }
      env += (ThreeLDots.name, LDotList(callArgs.toList))
      a
    }

    if (unTaggedForProcess.size > f.size) error(" unused parameters present ")
    else if (unTaggedForProcess.size > 0) (unTaggedForProcess zip f) foreach (arg => arg match {
      case (a: CallArg, h) => env += (h.name, new Evaluator(fEnv) eval a.e)
      case (a, h: DeclArgDef) if (a == NoneArg) => env += (h.name, defaultEvaluator eval h.default)
      case _ => // an error can come up when a variable is requested
    })
    // evaluate default parameters left  todo to check s (...)
    evalRemaining((f drop unTaggedForProcess.size) ::: s, env)
  }

  /**
   * Evaluates the rest of the declared arguments that weren't matched
   */
  private def evalRemaining(declArgs: List[FDeclArg], env: Environment) {
    declArgs foreach (_ match {
      case p: DeclArgDef => env += (p.name, defaultEvaluator eval p.default)
      case _ => // do nothing. An error can come up when a variable is requested but not found
    })
  }
}