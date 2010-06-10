package uk.ac.ebi.jr
package interpreter

import model.{Type, RObject}

/**
 *
 * Date: 30.05.2010
 * @author Taalai Djumabaev
 */
object Interpreter {
  def interpret(tree: Expression): (Any, Environment) = {
    interpret(tree, new Environment(collection.mutable.Map(), None))
  }

  def interpret(tree: Expression, env: Environment): (Any, Environment) = {
    new Interpreter(env).interpret(tree)
  }
}

class Interpreter(mainEnv: Environment) {
  def interpret(tree: Expression): (Any, Environment) = {
    val evaluator = new Evaluator(mainEnv += ("println", Println))
    (evaluator.eval(tree), evaluator.env)
  }

  trait RFunction[T] extends (List[T] => Any) {
    //all functions will behave like closures for now
    val objectType = Type.CLOSURE

    def formals //not decided the type yet

    def body: Expression

    def environment: Environment
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

    def apply(args: List[Any]) = args match {
      case (v: String) :: Nil => {println("\"" + v + "\""); v}
      case v :: Nil => {println(v); printLines += v.toString; v}
      case _ => error("println needs 1 argument")
    }
  }


  case class Closure(params: List[FDeclArg], expr: Expression, env: Environment)
      extends RObject with RFunction[FCallArg] {
    import scala.collection.mutable.Map
    import scala.collection.mutable.ListBuffer

    def formals = params

    def body = expr

    def environment = env

    def apply(args: List[FCallArg]) = {
      val evaluator = new Evaluator(new Environment(Map[String, Any](), Some(env)))
      val currParams = new ListBuffer[FDeclArg]() ++ params
      val diffTaggedArgs = new ListBuffer[CallArgDef]()
      val unTaggedArgs = new ListBuffer[FCallArg]()

      def updateProcessedArgs(call: CallArgDef, decl: FDeclArg) {
        evaluator.env += (decl.name, evaluator.eval(call.e))
        currParams -= decl
      }

      //argument matching
      args foreach (a => a match {
        case arg: CallArgDef => currParams filter (_.name == arg.name) toList match {
          case found :: Nil => updateProcessedArgs(arg, found)
          case List() => diffTaggedArgs + arg
           //can't get here since parser checks for same tags
          case _ => error("argument tag satisfies to more than one tags: " + arg.name)
        }
        case argument => unTaggedArgs += a
      })
      //partial matching. When a "..." parameter reached matching is stopped
      val forPartialMatching = currParams.toList.takeWhile(_.name != "...")
      diffTaggedArgs foreach (arg =>
        forPartialMatching filter (_.name.startsWith(arg.name)) toList match {
          case found :: Nil => updateProcessedArgs(arg, found)
          case List() => error("unknown parameter tag: " + arg.name)
          case _ => error("argument tag satisfies to more than one tags: " + arg.name)
        })
      //positional matching
      val (f, s) = currParams.toList span (_.name != "...")
      val unTaggedAsList = unTaggedArgs toList
      val unTaggedForProcess = if (s.isEmpty) unTaggedAsList else {
        val (a, b) = unTaggedAsList splitAt f.size
        evaluator.env += ("...", b)
        a
      }
      if (unTaggedForProcess.size > f.size) error("unused parameters present ")
      (unTaggedForProcess zip f) foreach (arg => arg match {
          case (a: CallArg, h) => evaluator.env += (h.name, evaluator.eval(a.e))
          case (a, h: DeclArgDef) if (a == NoneArg) =>
            evaluator.env += (h.name, evaluator.eval(h.default))
          case _ => // an error can come up when a variable is requested
      })

      // evaluate default parameters left
      (f drop unTaggedForProcess.size) ::: s foreach (_ match {
        case p: DeclArgDef => evaluator.env += (p.name, evaluator.eval(p.default))
        case _ => // do nothing. An error can come up when a variable is requested but not found
      })
      evaluator.eval(expr)
    }

    override def toString() = "Closure"
  }

  //class for evaluating with the environment
  class Evaluator(environment: Environment) {
    def env = environment

    def eval(e: Expression): Any = e match {

      case Block(l) => l.foldLeft((): Any) { (p, n) => eval(n) } // to check

      case IfStructure(If(ic, is), elseIfs, _else) => eval(ic) match {
        case true => eval(is)
        case false => {
          for (ElseIf(c, s) <- elseIfs) eval(c) match {
            case true => return eval(s)
            case false => // do nothing
            case _ => error("Not a boolean value in ElseIf expression")
          }
          _else match {
            case Some(Else(es)) => eval(es)
            case _ => None
          } //todo to be RObject NULL
        }
        case e => error("Not a boolean value in If expression : " + e) // todo toLogical interface usage to be added
      }

      case While(c, l) => {
        while (eval(c) match {
          case true => true
          case false => return NULL
          case _ => error("Not a boolean value in While expression")
        }) eval(l)
        NULL
      }

      case FunDecl(p, l) => Closure(p, l, env)

      case FunCall(func, args) => {
        val callArgs = if (args forall (_.isInstanceOf[FCallArg])) {
          args map (_.asInstanceOf[FCallArg])
        } else error("not an argument for the function apply method")
        eval(func) match {
          case c: Closure => c(callArgs)
          case f: RFunction[Any] => f(callArgs)
          case lit: String => env.resolve(lit) match {
            case Some(c: Closure) => c(callArgs)
            case Some(f: RFunction[Any]) => f(callArgs)
            case _ => error("Attempting to apply non-function value")
          }
          case a => {
            //A STRANGE MISTAKE IN GUI
            if (a.isInstanceOf[Closure]) a.asInstanceOf[Closure](callArgs)
            else error("Attempting to apply non-function value " + a)
          }
        }
      }

      // case CallArg(exp) => eval(exp) since for now all functions behave like closures

      case Var(id) => env.resolve(id) match {
        case Some(x) => x
        case None => error("Undefined var " + id)
      }

      case Lit(v) => v
      case Num(n) => n
      case Complex(c) => -1 // todo

      case True() => true // todo maybe I should leave True object without evaluating
      case False() => false

      case Add(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => i1 + i2
        case (a, b) => error("Unsupported operation for '+' " + a + " " + b)
      }

      case Subtract(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => i1 - i2
        case _ => error("Unsupported operation for '-'")
      }

      case Mul(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => i1 * i2
        case _ => error("Unsupported operation for '*'")
      }

      case Div(l, r) => (eval(l), eval(r)) match {
      //todo division by zero should lead to Inf object
        case (i1: Int, i2: Int) => i1 / i2
        case _ => error("Unsupported operation for '/'")
      }

      case Pow(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => Math.pow(i1, i2).toInt
        case _ => error("Unsupported operation for '^'")
      }

      //   case Sequence(l, r) =>

      case UserDefOp(n, l, r) => (env.resolve(n), l, r) match {
        case (Some(c: Closure), left: FCallArg, right: FCallArg) => c(List(left, right))
        //since all functions behave like closures for now
        case (Some(f: RFunction[Any]), left: FCallArg, right: FCallArg) => f(List(left, right))
        case _ => error("Undefined operation " + n)
      }

      case GreaterOrEq(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => i1 >= i2
        //add some booleans and vectors and so on
        case _ => error("Unsupported operation for '>='")
      }

      case Greater(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => i1 > i2
        //add some booleans and vectors and so on
        case _ => error("Unsupported operation for '>'")
      }

      case LessOrEq(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => i1 <= i2
        //add some booleans and vectors and so on
        case _ => error("Unsupported operation for '<='")
      }

      case Less(l, r) => (eval(l), eval(r)) match {
        case (i1: Int, i2: Int) => i1 < i2
        //add some booleans and vectors and so on
        case _ => error("Unsupported operation for '<'")
      }

      case Eq(l, r) => {
        //println(eval(l, con.child)._1.isInstanceOf[String]);
        //println(eval(r, con.child)._1.isInstanceOf[Int]);
        eval(l) == eval(r)
      }
      case NotEq(l, r) => eval(l) != eval(r)

      case And(l, r) => (eval(l), eval(r)) match {
        case (bl: Boolean, br: Boolean) => bl & br
        // add sth like 0 & 4 => 0
        case _ => error("Unsupported operation for '&&'")
      }

      //    case AndVectorized(l, r) =>
      //    case Or(l, r) =>
      //    case OrVectorized(l, r)  =>
      //    case Tilde(l, r)  =>

      //no evaluation of left part is performed.
      case Assign(l, r) => val v = eval(r);
      l match {
        case Var(id) => env += (id, v); v
        case Lit(l) => env += (l, v); v
        case _ => error("wrong left part of the assignment ")
      }


      //    case AssignToRight(l, r)  =>
      //    case Assign2ToRight(l, r) =>
      //    case AssignToLeft(l, r)   =>
      //    case Assign2ToLeft(l, r)  =>

      case UnPlus(e) => eval(e)
      case UnMinus(e) => eval(e) match {
        case i: Int => -i
        case _ => error("Unsupported operation for unary '-'")
      }

      case UnNot(e) => eval(e) match {
        case i: Int => if (i != 0) false else true // if I leave True instead of true then I need to change it here
        case b: Boolean => !b
        case _ => error("Unsupported operation for '!'")
      }

      //   case UnTilde(e) =>

      //   case Index(e, os) =>
      //   case DIndex(e, s) =>

      //todo
      //   case Next()  =>
      //   case Break() =>
      //   case Inf() =>


      case a => error("unsupported expression got. " + a.getClass)
    }
  }
}