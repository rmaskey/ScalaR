/*
 * Copyright (c) 2009-2010 European Molecular Biology Laboratory
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ebi.sr
package interpreter

import model.RVal._
import functions.Operations._
import functions._
import model._

/**
 * Interpreter of the parsed trees.
 * In fact, just wraps an evaluator
 *
 * Date: 30.05.2010
 * @author Taalai Djumabaev
 */
object Interpreter {
  def interpret(tree: Expression): (Any, Environment) = {
    interpret(tree, Environment.emptyEnv)
  }

  def interpret(tree: Expression, env: Environment): (Any, Environment) = {
    new Interpreter(env).interpret(tree)
  }
}

class Interpreter(mainEnv: Environment) {
  def interpret(tree: Expression): (Any, Environment) = {
    val evaluator = new Evaluator(mainEnv)
    (evaluator.eval(tree), evaluator.env)
  }
}

/**
 * class for evaluating with the environment. The parsed tree is walked through.
 */
class Evaluator(val env: Environment, session: RSession = RSession.currentSession) { //todo should be changed so that not only one session is allowed
  import functions.Subset._

  /**
   * main recursive evaluating method.
   *
   * @param e expression to be evaluated
   */
  def eval(e: Expression): RObject = e match {

    case Block(l) => l.init.foreach(eval(_)); eval(l.last)

    case IfStructure(If(ic, is), elseIfs, _else) => {
      val bool = AsLogical(eval(ic))
      if (bool.isEmpty) error("argument is of length zero")
      if (bool.length > 1) {}// warning
      bool.s(0) match {
        case RBool.NA => error("missing value where TRUE/FALSE needed")
        case 1 => return eval(is)
        case 0 => for (ElseIf(c, s) <- elseIfs) {
          val bool = AsLogical(eval(c))
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
        val bool = AsLogical(eval(c))
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

    case FunDecl(p, l) => {
      env.isBound_=(true)
      Closure(p, l, env)
    }

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

    case Index(e, s) => eval(e) match {
      case seq: Sequential[_] => `[`(seq, evalIndexArgs(s))(seq.m)
      case o => error(o.`type` + " is not subsettable")
    }
    case DIndex(e, s) => error("unimplemented operation [[ ")//todo

    case ExtractProperty(e, n) => eval(e) match {
      case l: RList => l.extract(n)
      case v: RVal[Any] => error("$ operator is invalid for atomic vectors")
      case o => error("object of type " + o.`type` + " is not subsettable")
    }

    case Add(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool =>    sum(lhs, rhs)
        case rhs: RInt =>     sum(lhs, rhs)
        case rhs: RDouble =>  sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool =>    sum(lhs, rhs)
        case rhs: RInt =>     sum(lhs, rhs)
        case rhs: RDouble =>  sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool =>    sum(lhs, rhs)
        case rhs: RInt =>     sum(lhs, rhs)
        case rhs: RDouble =>  sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool =>    sum(lhs, rhs)
        case rhs: RInt =>     sum(lhs, rhs)
        case rhs: RDouble =>  sum(lhs, rhs)
        case rhs: RComplex => sum(lhs, rhs)
        case _ => error("Unsupported operation for '+' ")
      }
      case _ => error("Unsupported operation for '+' ")
    }

    case Subtract(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool =>    subtract(lhs, rhs)
        case rhs: RInt =>     subtract(lhs, rhs)
        case rhs: RDouble =>  subtract(lhs, rhs)
        case rhs: RComplex => subtract(lhs, rhs)
        case _ => error("Unsupported operation for '-' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool =>    subtract(lhs, rhs)
        case rhs: RInt =>     subtract(lhs, rhs)
        case rhs: RDouble =>  subtract(lhs, rhs)
        case rhs: RComplex => subtract(lhs, rhs)
        case _ => error("Unsupported operation for '-' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool =>    subtract(lhs, rhs)
        case rhs: RInt =>     subtract(lhs, rhs)
        case rhs: RDouble =>  subtract(lhs, rhs)
        case rhs: RComplex => subtract(lhs, rhs)
        case _ => error("Unsupported operation for '-' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool =>    subtract(lhs, rhs)
        case rhs: RInt =>     subtract(lhs, rhs)
        case rhs: RDouble =>  subtract(lhs, rhs)
        case rhs: RComplex => subtract(lhs, rhs)
        case _ => error("Unsupported operation for '-' ")
      }
      case _ => error("Unsupported operation for '-' ")
    }

    case Mul(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool =>    multiply(lhs, rhs)
        case rhs: RInt =>     multiply(lhs, rhs)
        case rhs: RDouble =>  multiply(lhs, rhs)
        case rhs: RComplex => multiply(lhs, rhs)
        case _ => error("Unsupported operation for '*' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool =>    multiply(lhs, rhs)
        case rhs: RInt =>     multiply(lhs, rhs)
        case rhs: RDouble =>  multiply(lhs, rhs)
        case rhs: RComplex => multiply(lhs, rhs)
        case _ => error("Unsupported operation for '*' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool =>    multiply(lhs, rhs)
        case rhs: RInt =>     multiply(lhs, rhs)
        case rhs: RDouble =>  multiply(lhs, rhs)
        case rhs: RComplex => multiply(lhs, rhs)
        case _ => error("Unsupported operation for '*' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool =>    multiply(lhs, rhs)
        case rhs: RInt =>     multiply(lhs, rhs)
        case rhs: RDouble =>  multiply(lhs, rhs)
        case rhs: RComplex => multiply(lhs, rhs)
        case _ => error("Unsupported operation for '*' ")
      }
      case _ => error("Unsupported operation for '*' ")
    }

    //todo division by zero should lead to Inf object
    case Div(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool =>    divide(lhs, rhs)
        case rhs: RInt =>     divide(lhs, rhs)
        case rhs: RDouble =>  divide(lhs, rhs)
        case rhs: RComplex => divide(lhs, rhs)
        case _ => error("Unsupported operation for '/' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool =>    divide(lhs, rhs)
        case rhs: RInt =>     divide(lhs, rhs)
        case rhs: RDouble =>  divide(lhs, rhs)
        case rhs: RComplex => divide(lhs, rhs)
        case _ => error("Unsupported operation for '/' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool =>    divide(lhs, rhs)
        case rhs: RInt =>     divide(lhs, rhs)
        case rhs: RDouble =>  divide(lhs, rhs)
        case rhs: RComplex => divide(lhs, rhs)
        case _ => error("Unsupported operation for '/' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool =>    divide(lhs, rhs)
        case rhs: RInt =>     divide(lhs, rhs)
        case rhs: RDouble =>  divide(lhs, rhs)
        case rhs: RComplex => divide(lhs, rhs)
        case _ => error("Unsupported operation for '/' ")
      }
      case _ => error("Unsupported operation for '/' ")
    }

    case Pow(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool =>    pow(lhs, rhs)
        case rhs: RInt =>     pow(lhs, rhs)
        case rhs: RDouble =>  pow(lhs, rhs)
        case rhs: RComplex => pow(lhs, rhs)
        case _ => error("Unsupported operation for '^' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool =>    pow(lhs, rhs)
        case rhs: RInt =>     pow(lhs, rhs)
        case rhs: RDouble =>  pow(lhs, rhs)
        case rhs: RComplex => pow(lhs, rhs)
        case _ => error("Unsupported operation for '^' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool =>    pow(lhs, rhs)
        case rhs: RInt =>     pow(lhs, rhs)
        case rhs: RDouble =>  pow(lhs, rhs)
        case rhs: RComplex => pow(lhs, rhs)
        case _ => error("Unsupported operation for '^' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool =>    pow(lhs, rhs)
        case rhs: RInt =>     pow(lhs, rhs)
        case rhs: RDouble =>  pow(lhs, rhs)
        case rhs: RComplex => pow(lhs, rhs)
        case _ => error("Unsupported operation for '^' ")
      }
      case _ => error("Unsupported operation for '^' ")
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

    case GreaterOrEq(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => gteq(lhs, rhs)
        case rhs: RInt => gteq(lhs, rhs)
        case rhs: RDouble => gteq(lhs, rhs)
        case rhs: RChar => gteq(lhs, rhs)
        case _ => error("Unsupported operation for '>=' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => gteq(lhs, rhs)
        case rhs: RInt => gteq(lhs, rhs)
        case rhs: RDouble => gteq(lhs, rhs)
        case rhs: RChar => gteq(lhs, rhs)
        case _ => error("Unsupported operation for '>=' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => gteq(lhs, rhs)
        case rhs: RInt => gteq(lhs, rhs)
        case rhs: RDouble => gteq(lhs, rhs)
        case rhs: RChar => gteq(lhs, rhs)
        case _ => error("Unsupported operation for '>=' ")
      }
      case lhs: RChar => eval(r) match {
        case rhs: RBool => gteq(lhs, rhs)
        case rhs: RInt => gteq(lhs, rhs)
        case rhs: RDouble => gteq(lhs, rhs)
        case rhs: RChar => gteq(lhs, rhs)
        case _ => error("Unsupported operation for '>=' ")
      }
      case _ => error("Unsupported operation for '>=' ")
    }

    case Greater(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => gt(lhs, rhs)
        case rhs: RInt => gt(lhs, rhs)
        case rhs: RDouble => gt(lhs, rhs)
        case rhs: RChar => gt(lhs, rhs)
        case _ => error("Unsupported operation for '>' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => gt(lhs, rhs)
        case rhs: RInt => gt(lhs, rhs)
        case rhs: RDouble => gt(lhs, rhs)
        case rhs: RChar => gt(lhs, rhs)
        case _ => error("Unsupported operation for '>' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => gt(lhs, rhs)
        case rhs: RInt => gt(lhs, rhs)
        case rhs: RDouble => gt(lhs, rhs)
        case rhs: RChar => gt(lhs, rhs)
        case _ => error("Unsupported operation for '>' ")
      }
      case lhs: RChar => eval(r) match {
        case rhs: RBool => gt(lhs, rhs)
        case rhs: RInt => gt(lhs, rhs)
        case rhs: RDouble => gt(lhs, rhs)
        case rhs: RChar => gt(lhs, rhs)
        case _ => error("Unsupported operation for '>' ")
      }
      case _ => error("Unsupported operation for '>' ")
    }

    case LessOrEq(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => lteq(lhs, rhs)
        case rhs: RInt => lteq(lhs, rhs)
        case rhs: RDouble => lteq(lhs, rhs)
        case rhs: RChar => lteq(lhs, rhs)
        case _ => error("Unsupported operation for '<=' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => lteq(lhs, rhs)
        case rhs: RInt => lteq(lhs, rhs)
        case rhs: RDouble => lteq(lhs, rhs)
        case rhs: RChar => lteq(lhs, rhs)
        case _ => error("Unsupported operation for '<=' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => lteq(lhs, rhs)
        case rhs: RInt => lteq(lhs, rhs)
        case rhs: RDouble => lteq(lhs, rhs)
        case rhs: RChar => lteq(lhs, rhs)
        case _ => error("Unsupported operation for '<=' ")
      }
      case lhs: RChar => eval(r) match {
        case rhs: RBool => lteq(lhs, rhs)
        case rhs: RInt => lteq(lhs, rhs)
        case rhs: RDouble => lteq(lhs, rhs)
        case rhs: RChar => lteq(lhs, rhs)
        case _ => error("Unsupported operation for '<=' ")
      }
      case _ => error("Unsupported operation for '<=' ")
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

    case Eq(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => eql(lhs, rhs)
        case rhs: RInt => eql(lhs, rhs)
        case rhs: RDouble => eql(lhs, rhs)
        case rhs: RChar => eql(lhs, rhs)
        case _ => error("Unsupported operation for '==' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => eql(lhs, rhs)
        case rhs: RInt => eql(lhs, rhs)
        case rhs: RDouble => eql(lhs, rhs)
        case rhs: RChar => eql(lhs, rhs)
        case _ => error("Unsupported operation for '==' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => eql(lhs, rhs)
        case rhs: RInt => eql(lhs, rhs)
        case rhs: RDouble => eql(lhs, rhs)
        case rhs: RChar => eql(lhs, rhs)
        case _ => error("Unsupported operation for '==' ")
      }
      case lhs: RChar => eval(r) match {
        case rhs: RBool => eql(lhs, rhs)
        case rhs: RInt => eql(lhs, rhs)
        case rhs: RDouble => eql(lhs, rhs)
        case rhs: RChar => eql(lhs, rhs)
        case _ => error("Unsupported operation for '==' ")
      }
      case _ => error("Unsupported operation for '==' ")
    }

    case NotEq(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => neq(lhs, rhs)
        case rhs: RInt => neq(lhs, rhs)
        case rhs: RDouble => neq(lhs, rhs)
        case rhs: RChar => neq(lhs, rhs)
        case _ => error("Unsupported operation for '!=' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => neq(lhs, rhs)
        case rhs: RInt => neq(lhs, rhs)
        case rhs: RDouble => neq(lhs, rhs)
        case rhs: RChar => neq(lhs, rhs)
        case _ => error("Unsupported operation for '!=' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => neq(lhs, rhs)
        case rhs: RInt => neq(lhs, rhs)
        case rhs: RDouble => neq(lhs, rhs)
        case rhs: RChar => neq(lhs, rhs)
        case _ => error("Unsupported operation for '!=' ")
      }
      case lhs: RChar => eval(r) match {
        case rhs: RBool => neq(lhs, rhs)
        case rhs: RInt => neq(lhs, rhs)
        case rhs: RDouble => neq(lhs, rhs)
        case rhs: RChar => neq(lhs, rhs)
        case _ => error("Unsupported operation for '!=' ")
      }
      case _ => error("Unsupported operation for '!=' ")
    }


    case And(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '&&'")
    }

    case AndVectorized(l, r) => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => andVec(lhs, rhs)
        case rhs: RInt => andVec(lhs, rhs)
        case rhs: RDouble => andVec(lhs, rhs)
        case rhs: RComplex => andVec(lhs, rhs)
        case _ => error("Unsupported operation for '&' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => andVec(lhs, rhs)
        case rhs: RInt => andVec(lhs, rhs)
        case rhs: RDouble => andVec(lhs, rhs)
        case rhs: RComplex => andVec(lhs, rhs)
        case _ => error("Unsupported operation for '&' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => andVec(lhs, rhs)
        case rhs: RInt => andVec(lhs, rhs)
        case rhs: RDouble => andVec(lhs, rhs)
        case rhs: RComplex => andVec(lhs, rhs)
        case _ => error("Unsupported operation for '&' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool => andVec(lhs, rhs)
        case rhs: RInt => andVec(lhs, rhs)
        case rhs: RDouble => andVec(lhs, rhs)
        case rhs: RComplex => andVec(lhs, rhs)
        case _ => error("Unsupported operation for '&' ")
      }
      case _ => error("Unsupported operation for '&' ")
    }

    case Or(l, r) => (eval(l), eval(r)) match {
      case _ => error("Unsupported operation for '&&'")
    }

    case OrVectorized(l, r)  => eval(l) match {
      case lhs: RBool => eval(r) match {
        case rhs: RBool => orVec(lhs, rhs)
        case rhs: RInt => orVec(lhs, rhs)
        case rhs: RDouble => orVec(lhs, rhs)
        case rhs: RComplex => orVec(lhs, rhs)
        case _ => error("Unsupported operation for '|' ")
      }
      case lhs: RInt => eval(r) match {
        case rhs: RBool => orVec(lhs, rhs)
        case rhs: RInt => orVec(lhs, rhs)
        case rhs: RDouble => orVec(lhs, rhs)
        case rhs: RComplex => orVec(lhs, rhs)
        case _ => error("Unsupported operation for '|' ")
      }
      case lhs: RDouble => eval(r) match {
        case rhs: RBool => orVec(lhs, rhs)
        case rhs: RInt => orVec(lhs, rhs)
        case rhs: RDouble => orVec(lhs, rhs)
        case rhs: RComplex => orVec(lhs, rhs)
        case _ => error("Unsupported operation for '|' ")
      }
      case lhs: RComplex => eval(r) match {
        case rhs: RBool => orVec(lhs, rhs)
        case rhs: RInt => orVec(lhs, rhs)
        case rhs: RDouble => orVec(lhs, rhs)
        case rhs: RComplex => orVec(lhs, rhs)
        case _ => error("Unsupported operation for '|' ")
      }
      case _ => error("Unsupported operation for '|' ")
    }

    //    case Tilde(l, r)  =>

    case Assign(l, r) => val v = eval(r);
    l match {
      case Var(id) => {
        env.resolveLocal(id) match {
          case Some(x: RObject) => x.removeReferencer
          case None => // do nothing
        }
        env += (id, v); v
      }
      case Lit(lit) => {
        env.resolveLocal(lit) match {
          case Some(x: RObject) => x.removeReferencer
          case None => // do nothing
        }
        env += (lit, v); v
      }
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => built `<-`(a, env, v); v
        case _ => error("couldn't find function " + f + "<-") //todo
      }
      case Index(e, s) => `[<-`(e, evalIndexArgs(s), env, v); v
      case _ => error("wrong left part of the assignment ")
    }

    case AssignToRight(l, r)  => val v = eval(l);
    r match {
      case Var(id) => {
        env.resolveLocal(id) match {
          case Some(x: RObject) => x.removeReferencer
          case None => // do nothing
        }
        env += (id, v); v
      }
      case Lit(lit) => {
        env.resolveLocal(lit) match {
          case Some(x: RObject) => x.removeReferencer
          case None => // do nothing
        }
        env += (lit, v); v
      }
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => built `<-`(a, env, v); v
        case _ => error("couldn't find function " + f + "<-") //todo
      }
      case Index(e, s) => `[<-`(e, evalIndexArgs(s), env, v); v
      case _ => error("wrong right part of the assignment ")
    }

    case Assign2ToRight(l, r) => {
      val v = eval(l);
      r match {
        case Var(id) => env.resolveWithEnv(id) match {
          case Some((i, e: Environment)) => {
            i.removeReferencer
            e += (id, v)
          }
          case None => env += (id, v)
        }
        case Lit(lit) => env.resolveWithEnv(lit) match {
          case Some((i, e: Environment)) => {
            i.removeReferencer
            e += (lit, v)
          }
          case None => env += (lit, v)
        }
        case _ => error("wrong right part of the assignment ")
      }
      v
    }

    case AssignToLeft(l, r) => val v = eval(r);
    l match {
      case Var(id) => {
        env.resolveLocal(id) match {
          case Some(x: RObject) => x.removeReferencer
          case None => // do nothing
        }
        env += (id, v); v
      }
      case Lit(lit) => {
        env.resolveLocal(lit) match {
          case Some(x: RObject) => x.removeReferencer
          case None => // do nothing
        }
        env += (lit, v); v
      }
      case FunCall(f, a) => eval(f) match {
        case built: Assignable => built `<-`(a, env, v); v
        case _ => error("couldn't find function " + f + "<-") //todo
      }
      case Index(e, s) => `[<-`(e, evalIndexArgs(s), env, v); v
      case _ => error("wrong left part of the assignment ")
    }

    case Assign2ToLeft(l, r) => {
      val v = eval(r);
      l match {
        case Var(id) => env.resolveWithEnv(id) match {
          case Some((i, e: Environment)) => {
            i.removeReferencer
            e += (id, v)
          }
          case None => env += (id, v)
        }
        case Lit(lit) => env.resolveWithEnv(lit) match {
          case Some((i, e: Environment)) => {
            i.removeReferencer
            e += (lit, v)
          }
          case None => env += (lit, v)
        }
        case _ => error("wrong left part of the assignment ")
      }
      v
    }

    case UnPlus(e) => eval(e) match {
      case r: RVal[_] => r
      case o => error("invalid argument to unary operator")
    }
    
    case UnMinus(e) => eval(e) match {
      case _ => error("Unsupported operation for unary '-'")
    }

    case UnNot(e) => eval(e) match {
      case _ => error("Unsupported operation for '!'")
    }

    //   case UnTilde(e) =>

    //todo
    //   case Next  =>
    //   case Break =>
    //   case Inf =>

    //todo maybe should check for Nones and call error("... undefined ...")
    case DoubleColon(l, r) => session.loadedLibraries.get(l) match {
      case Some(x) => x.exported.get(r) match {
        case Some(y) => y
        case None => error("undefined var " + r + " in library " + l)
      }
      case None => error("undefined library: " + l)
    }
    case TripleColon(l, r) => session.loadedLibraries.get(l) match {
      case Some(x) => x.env.resolve(r) match {
        case Some(y) => y
        case None => error("undefined var " + r + " in library " + l)
      }
      case None => error("undefined library: " + l)
    }
    case a => error("unsupported expression got. " + a)
  }

  /**
   * evaluates argument passed to subsetting
   */
  def evalIndexArgs(l: List[IndexArgument]) = for (i <- l) yield (i match {
    case em @ EmptyIndex => em
    case IndexArg(expr) => eval(expr) })
}

object Evaluator {
  def eval(e: Expression, env: Environment): RObject = new Evaluator(env).eval(e)
}