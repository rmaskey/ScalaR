package uk.ac.ebi.sr
package interpreter

import model.RVal._
import model.Operations._
import model.{Environment, Println, Builtin, RFunction, Closure, Assignable, Attributes, Attr, Length}

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
              "println" -> Println,
              "attr" -> Attr,
              "attributes" -> Attributes,
              "length" -> Length))
    (evaluator.eval(tree), evaluator.env)
  }
}
  //class for evaluating with the environment
class Evaluator(environment: Environment) {
  def env = environment

  def eval(e: Expression): Any = e match {

    // foreach is not used here because the last value of expression in block is returned
    case Block(l) => l.foldLeft((): Any) { (p, n) => eval(n) }

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
          case _ =>
        }
        NULL
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
        case f: RFunction[FCallArg] => f(callArgs, env)
        case lit: RChar => env.resolve(lit.s(0)) match {
          case Some(f: RFunction[FCallArg]) => f(callArgs, env)
          case _ => error("Attempting to apply non-function value")
        }
        case a => {
          //A STRANGE BUG IN GUI
          if (a.isInstanceOf[RFunction[FCallArg]]) a.asInstanceOf[RFunction[FCallArg]](callArgs, env)
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

    case True => true // todo maybe I should leave True object without evaluating
    case False => false
    case NULL => NULL

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
      case (i1: Int, i2: Int) => math.pow(i1, i2).toInt
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
      case Some(f: RFunction[FCallArg]) => f(List(l, r), env)
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
      case Lit(lit) => env += (lit, v); v
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => {
          val callArgs = if (a forall (_.isInstanceOf[FCallArg])) {
            a map (_.asInstanceOf[FCallArg])
          } else error("not an argument for the function apply method")
          built `<-`(callArgs, env, v)       ///env should be changed for evaluating the arguments but asssignments should be done into global one..
        }
      }
      case _ => error("wrong left part of the assignment ")
    }

    case AssignToRight(l, r)  => val v = eval(l);
    r match {
      case Var(id) => env += (id, v); v
      case Lit(lit) => env += (lit, v); v
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => {
          val callArgs = if (a forall (_.isInstanceOf[FCallArg])) {
            a map (_.asInstanceOf[FCallArg])
          } else error("not an argument for the function apply method")
          built `<-`(callArgs, env, v)       ///env should be changed for evaluating the arguments but asssignments should be done into global one..
        }
      }
      case _ => error("wrong right part of the assignment ")
    }

    case Assign2ToRight(l, r) => {
      val v = eval(l);
      r match {
        case Var(id) => env.resolve(id, true) match {
          case Some((i, e: Environment)) => e += (id, v)
          case None => env += (id, v)
        }
        case Lit(lit) => env.resolve(lit, true) match {
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
        case built: Assignable => {
          val callArgs = if (a forall (_.isInstanceOf[FCallArg])) {
            a map (_.asInstanceOf[FCallArg])
          } else error("not an argument for the function apply method")
          built `<-`(callArgs, env, v)       ///env should be changed for evaluating the arguments but asssignments should be done into global one..
        }
      }
      case _ => error("wrong left part of the assignment ")
    }

    case Assign2ToLeft(l, r) => {
      val v = eval(r);
      l match {
        case Var(id) => env.resolve(id, true) match {
          case Some((i, e: Environment)) => e += (id, v)
          case None => env += (id, v)
        }
        case Lit(lit) => env.resolve(lit, true) match {
          case Some((i, e: Environment)) => e += (lit, v)
          case None => env += (lit, v)
        }
        case _ => error("wrong left part of the assignment ")
      }
      v
    }

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


    case a => error("unsupported expression got. " + a)
  }
}

object Evaluator {
  def eval(e: Expression, env: Environment): Any = new Evaluator(env).eval(e)
}