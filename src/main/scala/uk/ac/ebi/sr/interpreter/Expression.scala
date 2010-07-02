package uk.ac.ebi.sr
package interpreter

import model.{Type, RObject}
import model.RVal.RChar

/**
 *
 * Date: 19.03.2010
 * @author Taalai Djumabaev
 */

sealed abstract class Expression extends RObject {
  val `type` = Type.EXPRESSION
}

case class Block(val l: List[Expression]) extends Expression

case class IfStructure(_if: If, elseIf: List[ElseIf], _else: Option[Else]) extends Expression
case class If(condition: Expression, stats: Expression) extends Expression
case class ElseIf(condition: Expression, stats: Expression) extends Expression
case class Else(stats: Expression) extends Expression
case class While(condition: Expression, stats: Expression) extends Expression
case class Repeat(stats: Expression) extends Expression
case class For(_var: Var, expr: Expression, stats: Expression) extends Expression

case class Var(ident: String) extends Expression
case class Lit(lit: String) extends Expression
case class Num(v: Any) extends Expression

case class FunDecl(params: List[FDeclArg], stats: Expression) extends Expression {
  require(params.filter(_ == ThreeLDots).length <= 1)
}

abstract class FDeclArg(val name: String) extends Expression
case object ThreeLDots extends FDeclArg("...")
case class TwoLDots(num: Int) extends FDeclArg(".." + num)
case class DeclArg(override val name: String) extends FDeclArg(name)
case class DeclArgDef(override val name: String, default: Expression) extends FDeclArg(name)

abstract class FCallArg extends Expression
case class CallArg(e: Expression) extends FCallArg
case class CallArgDef(name: String, e: Expression) extends FCallArg
case object NoneArg extends FCallArg

case class FunCall(function: Expression, args: List[Expression]) extends Expression


case class Index(e: Expression, subset: List[Expression]) extends Expression
case class DIndex(e: Expression, subset: List[Expression]) extends Expression
case class ExtractProperty(e: Expression, name: String) extends Expression
case class ExtractSlot(e: Expression, name: String) extends Expression
case class DoubleColon(l: String, r: String) extends Expression
case class TripleColon(l: String, r: String) extends Expression


//class for [, 1] like indexes.
abstract class IndexArgument extends Expression
case object EmptyIndex extends IndexArgument
case class IndexArg(e: Expression) extends IndexArgument

case object True extends Expression
case object False extends Expression

//todo sth with this
case object Inf extends Expression

case class Add(l: Expression, r: Expression) extends Expression
case class Subtract(l: Expression, r: Expression) extends Expression
case class Mul(l: Expression, r: Expression) extends Expression
case class Div(l: Expression, r: Expression) extends Expression
case class Pow(l: Expression, r: Expression) extends Expression
case class Sequence(l: Expression, r: Expression) extends Expression
case class UserDefOp(name: String, l: FCallArg, r: FCallArg) extends Expression

case class GreaterOrEq(l: Expression, r: Expression) extends Expression
case class Greater(l: Expression, r: Expression) extends Expression
case class LessOrEq(l: Expression, r: Expression) extends Expression
case class Less(l: Expression, r: Expression) extends Expression
case class Eq(l: Expression, r: Expression) extends Expression
case class NotEq(l: Expression, r: Expression) extends Expression

case class Assign(l: Expression, r: Expression) extends Expression // =
case class AssignToLeft(l: Expression, r: Expression) extends Expression // <-
case class Assign2ToLeft(l: Expression, r: Expression) extends Expression // <<-
case class AssignToRight(l: Expression, r: Expression) extends Expression // ->
case class Assign2ToRight(l: Expression, r: Expression) extends Expression // ->>

case class AndVectorized(l: Expression, r: Expression) extends Expression // &
case class And(l: Expression, r: Expression) extends Expression // &&
case class OrVectorized(l: Expression, r: Expression) extends Expression // |
case class Or(l: Expression, r: Expression) extends Expression // ||
case class Tilde(l: Expression, r: Expression) extends Expression

case class UnPlus(e: Expression) extends Expression
case class UnMinus(e: Expression) extends Expression
case class UnNot(e: Expression) extends Expression
case class UnTilde(e: Expression) extends Expression

case object Next extends Expression
case object Break extends Expression

case object NULL extends Expression
case object EOfF extends Expression