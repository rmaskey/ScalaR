package uk.ac.ebi.sr
package interpreter

import model.RVal._
import functions.Operations._
import functions.{Attr, Attributes, Length, Builtin, RFunction, Closure, Assignable, Subset}
import model.{RObject, Environment}

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
    val evaluator = new Evaluator(mainEnv ++=
            List(
              "attr" -> Attr,
              "attributes" -> Attributes,
              "length" -> Length))
    (evaluator.eval(tree), evaluator.env)
  }
}
  //class for evaluating with the environment
class Evaluator(environment: Environment) {
  import functions.AsLogical._
  import functions.Subset._
  def env = environment

  def eval(e: Expression): RObject = e match {

    case Block(l) => l.init.foreach(eval(_)); eval(l.last)

    case IfStructure(If(ic, is), elseIfs, _else) => {
      val bool = `as.logical`(eval(ic))
      if (bool.isEmpty) error("argument is of length zero")
      if (bool.length > 1) {}// warning
      bool.s(0) match {
        case RBool.NA => error("missing value where TRUE/FALSE needed")
        case 1 => return eval(is)
        case 0 => for (ElseIf(c, s) <- elseIfs) {
          val bool = `as.logical`(eval(c))
          if (bool.isEmpty) error("argument is of length zero")
          if (bool.length > 1) {}// warning
          bool.s(0) match {
            case RBool.NA => error("missing value where TRUE/FALSE needed")
            case 1 => return eval(s)
            case 0 => // do nothing
            case _ => error("Not a boolean value in ElseIf expression")
          }
        }
        _else match {
          case Some(Else(es)) => eval(es)
          case _ => NULL
        }
        case _ => error("Not a boolean value in If expression")
      }
    }

    case While(c, l) => {
      while ({
        val bool = `as.logical`(eval(c))
        if (bool.isEmpty) error("argument is of length zero")
        if (bool.length > 1) {}// warning
        bool.s(0) match {
        case RBool.NA => error("missing value where TRUE/FALSE needed")
        case 1 => true
        case 0 => false
        case _ => error("Not a boolean value in While expression")
        }
      }) eval(l)
      NULL
    }

    case FunDecl(p, l) => Closure(p, l, env)

    case FunCall(func, args) => {
      eval(func) match {
        case f: RFunction => f(args, env)
        case lit: RChar => env.resolve(lit.s(0)) match {
          case Some(f: RFunction) => f(args, env)
          case _ => error("Attempting to apply non-function value")
        }
        case a => {
          //A STRANGE BUG IN GUI
          if (a.isInstanceOf[RFunction]) a.asInstanceOf[RFunction](args, env)
          else error("Attempting to apply non-function value " + a)
        }
      }
    }

    // case CallArg(exp) => eval(exp) since for now all functions behave like closures

    case Var(id) => env.resolve(id) match {
      case Some(x: Expression) => eval(x)
      case Some(x) => x
      case None => error("Undefined var " + id)
    }

    case Lit(v) => RChar(v)
    case Num(n) => n

    case True => RBool(1)
    case False => RBool(0)
    case NULL => NULL

    case Index(e, s) => `[`(eval(e), for (i <- s) yield { i match {
      case em @ EmptyIndex => em
      case IndexArg(expr) => eval(expr) }
    })
    case DIndex(e, s) => error("unimplemented operation [[ ")//todo


    case Add(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => sum(lhs, rhs)
        case rhs: RInt => sum(lhs, rhs)
        case rhs: RDouble => sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => sum(lhs, rhs)
        case rhs: RInt => sum(lhs, rhs)
        case rhs: RDouble => sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => sum(lhs, rhs)
        case rhs: RInt => sum(lhs, rhs)
        case rhs: RDouble => sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool => sum(lhs, rhs)
        case rhs: RInt => sum(lhs, rhs)
        case rhs: RDouble => sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case _ => error("Unsupported operation for '+' ")
    }

    case Subtract(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '-'")
    }

    case Mul(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '*'")
    }

    case Div(l, r) => (eval(l), eval(r)) match {
    //todo division by zero should lead to Inf object
      case _ => error("Unsupported operation for '/'")
    }

    case Pow(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '^'")
    }

    case Sequence(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => seq(lhs, rhs)
        case rhs: RInt => seq(lhs, rhs)
        case rhs: RDouble => seq(lhs, rhs)
        case rhs: RComplex => seq(lhs, rhs)
        case _ => error("Unsupported operation for ':' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => seq(lhs, rhs)
        case rhs: RInt => seq(lhs, rhs)
        case rhs: RDouble => seq(lhs, rhs)
        case rhs: RComplex => seq(lhs, rhs)
        case _ => error("Unsupported operation for ':' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => seq(lhs, rhs)
        case rhs: RInt => seq(lhs, rhs)
        case rhs: RDouble => seq(lhs, rhs)
        case rhs: RComplex => seq(lhs, rhs)
        case _ => error("Unsupported operation for ':' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool => seq(lhs, rhs)
        case rhs: RInt => seq(lhs, rhs)
        case rhs: RDouble => seq(lhs, rhs)
        case rhs: RComplex => seq(lhs, rhs)
        case _ => error("Unsupported operation for ':' ")
      }
      case _ => error("Unsupported operation for ':' ")
    }

    case UserDefOp(n, l, r) => env.resolve(n) match {
      case Some(f: RFunction) => f(List(l, r), env)
      case _ => error("Undefined operation " + n)
    }

    case GreaterOrEq(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '>='")
    }

    case Greater(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '>'")
    }

    case LessOrEq(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '<='")
    }

    case Less(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => lt(lhs, rhs)
        case rhs: RInt => lt(lhs, rhs)
        case rhs: RDouble => lt(lhs, rhs)
        case rhs: RChar => lt(lhs, rhs)
        case _ => error("Unsupported operation for '<' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => lt(lhs, rhs)
        case rhs: RInt => lt(lhs, rhs)
        case rhs: RDouble => lt(lhs, rhs)
        case rhs: RChar => lt(lhs, rhs)
        case _ => error("Unsupported operation for '<' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => lt(lhs, rhs)
        case rhs: RInt => lt(lhs, rhs)
        case rhs: RDouble => lt(lhs, rhs)
        case rhs: RChar => lt(lhs, rhs)
        case _ => error("Unsupported operation for '<' ")
      }
      case lhs: RChar => eval(r) match {
        case rhs: RBool => lt(lhs, rhs)
        case rhs: RInt => lt(lhs, rhs)
        case rhs: RDouble => lt(lhs, rhs)
        case rhs: RChar => lt(lhs, rhs)
        case _ => error("Unsupported operation for '<' ")
      }
      case _ => error("Unsupported operation for '<' ")
    }

    case Eq(l, r) => error("Unsupported operation '=' ")
    case NotEq(l, r) => error("Unsupported operation '!=' ")//eval(l) != eval(r)

    case And(l, r) => (eval(l), eval(r)) match {
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
      case Lit(lit) => env += (lit, v); v
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => built `<-`(a, env, v)
      }
      case _ => error("wrong left part of the assignment ")
    }

    case AssignToRight(l, r)  => val v = eval(l);
    r match {
      case Var(id) => env += (id, v); v
      case Lit(lit) => env += (lit, v); v
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => built `<-`(a, env, v)
      }
      case _ => error("wrong right part of the assignment ")
    }

    case Assign2ToRight(l, r) => {
      val v = eval(l);
      r match {
        case Var(id) => env.resolveWithEnv(id) match {
          case Some((i, e: Environment)) => e += (id, v)
          case None => env += (id, v)
        }
        case Lit(lit) => env.resolveWithEnv(lit) match {
          case Some((i, e: Environment)) => e += (lit, v)
          case None => env += (lit, v)
        }
        case _ => error("wrong right part of the assignment ")
      }
      v
    }

    case AssignToLeft(l, r) => val v = eval(r);
    l match {
      case Var(id) => env += (id, v); v
      case Lit(lit) => env += (lit, v); v
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => built `<-`(a, env, v)
      }
      case _ => error("wrong left part of the assignment ")
    }

    case Assign2ToLeft(l, r) => {
      val v = eval(r);
      l match {
        case Var(id) => env.resolveWithEnv(id) match {
          case Some((i, e: Environment)) => e += (id, v)
          case None => env += (id, v)
        }
        case Lit(lit) => env.resolveWithEnv(lit) match {
          case Some((i, e: Environment)) => e += (lit, v)
          case None => env += (lit, v)
        }
        case _ => error("wrong left part of the assignment ")
      }
      v
    }

    case UnPlus(e) => eval(e)
    case UnMinus(e) => eval(e) match {
      case _ => error("Unsupported operation for unary '-'")
    }

    case UnNot(e) => eval(e) match {
      case _ => error("Unsupported operation for '!'")
    }

    //   case UnTilde(e) =>

    //todo
    //   case Next()  =>
    //   case Break() =>
    //   case Inf() =>


    case a => error("unsupported expression got. " + a)
  }
}

object Evaluator {
  def eval(e: Expression, env: Environment): RObject = new Evaluator(env).eval(e)
}