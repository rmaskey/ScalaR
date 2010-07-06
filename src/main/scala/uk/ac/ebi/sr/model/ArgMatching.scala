package uk.ac.ebi.sr
package model

import interpreter._

/**
 *
 * Date: Jul 5, 2010
 * @author Taalai Djumabaev
 */
trait ArgMatching {
  import scala.collection.mutable.ListBuffer

  val params: List[FDeclArg]
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

  def evalArgs(args: List[FCallArg], fEnv: Environment) {
    val tagged = args filter (_.isInstanceOf[CallArgDef])
    val (declArgs, remaining) = if (tagged.size > 0)
      matchTags(tagged.asInstanceOf[List[CallArgDef]], params, fEnv) else (params, List())
    val unTagged = args filter (a => !a.isInstanceOf[CallArgDef] || remaining.exists (_ == a))
    if (unTagged.size > 0) matchPos(unTagged, declArgs, fEnv)
  }
}