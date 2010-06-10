package uk.ac.ebi.sr
package interpreter

import util.parsing.combinator.syntactical.StdTokenParsers
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq

/**
 *
 * Date: 21.03.2010
 * @author Taalai Djumabaev
 */

object RParser extends StdTokenParsers {
  type Tokens = RTokens
  val lexical = new RLexer

  import lexical.{DecimalNum, UserDefinedOperation, LitIdentifier,
    ComplexNum, NumericLit, Keyword, NewLineDelimiter}

  lexical.delimiters ++= List("{", "}", "(", ")", "[", "[[", "]", "]]", ",", "\t", ";", "\n", "\r",
      "::", "$", "@", "^", "-", "+", ":", ":::", "*", "/", ">", ">=", "<", "%",
      "<=", "==", "!=", "!", "&", "&&", "|", "||", "~", "->", "->>", "=", "<-", "<<-")

  // "..." and "..1" etc are reserved - they are predefined identifiers but cannot be used as object names
  lexical.reserved ++= List("for", "in", "while", "repeat", "break", "next", "function", "if", "else", "elseif",
    "TRUE", "FALSE", "Inf", "NULL", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_", "...", "..")

  def rProgram: RLangParser[Expression] = rep(newLineDelimiters) ~~> program

  def program: RLangParser[Expression] = (tightRepsep(expression, delimiters) ^^ Block) <~ (delimiters?)
  def delimiters = ";" | newLineDelimiters
  def newLineDelimiters = elem("new line delimiter", _ == NewLineDelimiter) ^^ (_.chars)

  def expression: RLangParser[Expression] =
    ( sStructure
    | funDecl
    | term
    | "break" ^^^ Break
    | "next" ^^^ Next
    )  //todo as variable | "Inf" ^^^ Inf)

  def sStructure: RLangParser[Expression] =
    ( ifStructure ~~ rep(elseIf) ~~ elseStructure ^^
        { case _if ~ elseIf ~ _else => IfStructure(_if, elseIf, _else) }
    | "while" ~> "(" ~> expression ~ ")" ~ expression ^^ { case cond ~ _ ~ stats => While(cond, stats) }
    | "repeat" ~> expression                          ^^ Repeat
    | "for" ~> "(" ~> ident ~ "in" ~ expression ~ ")" ~ expression ^^
        { case varName ~ _ ~ exp ~ _ ~ stats => For(Var(varName), exp, stats) }
    // | switch // is it a special structure - can be thought of as a funCall
    )

  def ifStructure: RLangParser[If] = "if" ~> "(" ~> expression ~ ")" ~ expression ^^
      { case cond ~ _ ~ stats => If(cond, stats) }

  def elseIf: RLangParser[ElseIf] =
    "elseif" ~> "(" ~> expression ~ ")" ~ expression ^^ { case cond ~ _ ~ stats => ElseIf(cond, stats) }

  def elseStructure: RLangParser[Option[Else]] =
    opt("else" ~> expression) ^^ { case Some(stats) => Some(Else(stats))
                                   case _ => None }

  def funDecl: RLangParser[Expression] = {
    val declGroup = new ArgumentGroup
    "function" ~> "(" ~> repsep(funDeclArg(declGroup), ",") ~ ")" ~ expression ^^
      { case params ~ _ ~ stats => FunDecl(params, stats) }
  }

  def funDeclArg(declGroup: ArgumentGroup): RLangParser[FDeclArg] =
    ( ident ~ opt("=" ~> expression) ^^ { case id ~ Some(exp) => declGroup.check(id); DeclArgDef(id, exp)
                                                case id ~ _ => declGroup.check(id); DeclArg(id) }
    | "..."              ^^ { case d => declGroup.check(d); ThreeLDots }
    | ".." ~> numericLit ^^ { case n => declGroup.check(n); TwoLDots(n.toInt) }
    )

  //precedence order is essential here
  def term: RLangParser[Expression] = assignL ~~ opt(("<<-" | "<-") ~ term) ^^
      { case assL ~ Some("<<-" ~ ter) => Assign2ToLeft(assL, ter)
        case assL ~ Some("<-" ~ ter)  => AssignToLeft(assL, ter)
        case assL ~ _ => assL }

   //todo *<- functions are searched if term is on the left
  def assignL: RLangParser[Expression] = assignEq ~~ opt("=" ~> assignL) ^^
      { case assE ~ Some(assL) => Assign(assE, assL)
        case assE ~ _ => assE }

  def assignEq = assignR * ("->>" ^^^ Assign2ToRight | "->" ^^^ AssignToRight)
  def assignR  = tildeUn * ("~" ^^^ Tilde)
  def tildeUn: RLangParser[Expression]  =
    ( "~" ~> tildeUn ^^ { case til => UnTilde(til) }
    | or * ("||" ^^^ Or | "|" ^^^ OrVectorized)
    )

  def or  = and * ("&&" ^^^ And | "&" ^^^ AndVectorized)

  // x < y < z are not allowed in compare operators
  def and: RLangParser[Expression] =
    ( "!" ~> and ^^ { case not => UnNot(not) }
    | compare ~~ opt((">=" | ">" | "<=" | "<" | "==" | "!=") ~ compare) ^^
        { case cl ~ Some(">=" ~ cr) => GreaterOrEq(cl, cr)
          case cl ~ Some(">"  ~ cr) => Greater(cl, cr)
          case cl ~ Some("<=" ~ cr) => LessOrEq(cl, cr)
          case cl ~ Some("<"  ~ cr) => Less(cl, cr)
          case cl ~ Some("==" ~ cr) => Eq(cl, cr)
          case cl ~ Some("!=" ~ cr) => NotEq(cl, cr)
          case c ~ _ => c }
    )

  def compare = add * ("+" ^^^ Add | "-" ^^^ Subtract)
  def add     = mul * ("*" ^^^ Mul | "/" ^^^ Div)
  def mul     = usrDef ~~ ((userDefinedOp ~ usrDef)*) ^^
      { case e ~ l => l.foldLeft(e) { case (e1, f ~ e2) => UserDefOp(f, CallArg(e1), CallArg(e2)) } }

  //having multiple ":" operators yields to a warning in R - not an error
  def usrDef  = range * (":" ^^^ Sequence)

  // operations are unary and ^ operation has right to left precedence
  def range: RLangParser[Expression] =
    ( "+" ~> range ^^ { case rang => UnPlus(rang) }
    | "-" ~> range ^^ { case rang => UnMinus(rang) }
    | pow ~~ opt("^" ~> range) ^^ { case _pow ~ Some(ran) => Pow(_pow, ran)
                                    case _pow ~ _ => _pow }
    )
  def pow = apply ~~ ((dIndex | index | args | extractProperty | extractSlot)*) ^^
      { case ap ~ subs => subs.foldLeft(ap) { case (v, "[[" ~ sub) => DIndex(v, sub)
                                              case (v, "[" ~ sub) => Index(v, sub)
                                              case (v, "(" ~ app) => FunCall(v, app)
                                              case (v, "$" ~ ext) => ExtractProperty(v, ext mkString)
                                              case (v, "@" ~ ext) => ExtractSlot(v, ext mkString) } }

  // None or "empty comma" arguments for "[[ ]]" will cause an error later in the interpreter
  def index:  RLangParser[String ~ List[Expression]] = "[" ~ repsep(indexArg, ",") <~ "]"
  def dIndex: RLangParser[String ~ List[Expression]] = "[[" ~ repsep(indexArg, ",") <~ "]]"
  def indexArg = (expression?) ^^ { case Some(x) => IndexArg(x)
                                    case _ => EmptyIndex }

  def args: RLangParser[String ~ List[FCallArg]] =
    "(" ~ repsep(opt(opt(name <~ "=") ~ expression) ^^
          { case Some(Some(id) ~ e) => CallArgDef(id, e)
            case Some(_ ~ e) => CallArg(e)
            case _ => NoneArg }, ",") <~ ")"

  def extractProperty = "$" ~ propertyName
  def extractSlot     = "@" ~ propertyName

  def propertyName    = name ^^ { case n => (Var(n) :: Nil) }

  def apply: RLangParser[Expression] =
    ( boolean
    | ident ~~ opt((":::" | "::") ~ name) ^^
        { case i ~ Some(":::" ~ n) => TripleColon(i, n)
          case i ~ Some("::" ~ n) => DoubleColon(i, n)
          case i ~ _ => Var(i) }
    | stringLit ~~ opt((":::" | "::") ~ name) ^^
        { case i ~ Some(":::" ~ n) => TripleColon(i, n)
          case i ~ Some("::" ~ n) => DoubleColon(i, n)
          case i ~ _ => Lit(i) }
    | "(" ~> expression <~ ")"
    | "{" ~> program <~ "}"
    | numericLit ^^ Num.compose(_.toInt)
    | decimalNumber ^^ Num.compose(_.toDouble)
    | complexNum ^^ Complex
    | funDecl
    )

  def name: RLangParser[String] = ident | stringLit
  def boolean = "TRUE" ^^^ True() | "FALSE" ^^^ False()

  def complexNum = elem("complex number", _.isInstanceOf[ComplexNum]) ^^
      ((_:Elem).asInstanceOf[ComplexNum].format match {
        case i: NumericLit => i.chars.toInt
        case d: DecimalNum => d.chars.toDouble
        case _ => error("undefined format of number")
      })

  def decimalNumber: RLangParser[String] = elem("decimal number", _.isInstanceOf[DecimalNum]) ^^ (_.chars)
  def userDefinedOp: RLangParser[String] = elem("string literal", _.isInstanceOf[UserDefinedOperation]) ^^ (_.toString)
  //needed because literals are identifiers in R except some special cases
  override def stringLit: RLangParser[String] = elem("literal identifier", _.isInstanceOf[LitIdentifier]) ^^ (_.chars)

  class ArgumentGroup {
    import collection.mutable.Set
    var decl: Set[String] = null

    def check(id: String) = {
      if (decl == null) decl = Set[String]()
      if (decl.contains(id)) error("duplicate identifier in function declaration")
      decl += id
    }
  }

  abstract class RLangParser[+T] extends Parser[T] { p =>
    def ~~ [U](q: => Parser[U]): Parser[~[T, U]] = (for(a <- this; b <- q) yield new ~(a,b)).named("~")
    def ~~> [U](q: => Parser[U]) = (p ~~ q) ^^ { case x ~ y => y }
    def <~~ [U](q: => Parser[U]) = (p ~~ q) ^^ { case x ~ y => x }

    override def ~ [U](q: => Parser[U]): RLangParser[T ~ U] = new RLangParser[T ~ U] {
      def apply(in: Input) = (p <~~ rep(newLineDelimiters))(in) match {
        case Success(x, in1) => q(in1) match {
          case Success(y, in2) => Success(new ~(x, y), in2)
          case Failure(m, in2) => Failure(m, in)
          case ns: NoSuccess => Failure("message of no success", in)
        }
        case ns: NoSuccess => ns
      }
    }
    override def ~> [U](q: => Parser[U]): RLangParser[U] = p ~ q ^^ { case x ~ y => y }
    override def <~ [U](q: => Parser[U]): RLangParser[T] = p ~ q ^^ { case x ~ y => x }

    override def * [U >: T](sep: => Parser[(U, U) => U]) = chainLeft(this, this, sep)
    def chainLeft[T, U](first: => Parser[T], p: => Parser[U], q: => Parser[(T, U) => T]): Parser[T] =
      first ~~ rep(q ~ p) ^^ { case x ~ xs => xs.foldLeft(x) {(_, _) match { case (a, f ~ b) => f(a, b) }}}

    override def map[U](f: T => U): RLangParser[U] = RLangParser{ in => this(in) map(f)}
    def RLangParser[T](f: Input => ParseResult[T]): RLangParser[T] =
      new RLangParser[T]{ def apply(in: Input) = f(in) }

    override def ^^ [U](f: T => U): RLangParser[U] = map(f)
    override def ^^^ [U](r: U): RLangParser[U] = ^^ (_ => r)
    override def | [U >: T](q: => Parser[U]): RLangParser[U] = append(q)
  }

  def tightRep[T](p: => RLangParser[T]): RLangParser[List[T]] =
    p ~~ rep(p) ^^ { case x ~ xs => x :: xs }

  def tightRepsep[T](p: => RLangParser[T], q: => RLangParser[Any]): RLangParser[List[T]] =
    p ~~ rep(q ~> p) ^^ { case r ~ rs => r :: rs } | success(List())

  override implicit def keyword(chars: String): RLangParser[String] = accept(Keyword(chars)) ^^ (_.chars)
  implicit def parserToRLangParser [T](p: Parser[T]): RLangParser[T] =
    new RLangParser[T] { q => def apply(in: Input) = p(in) }

  def main(args: Array[String]) {
    //val input = new PagedSeqReader(PagedSeq.fromFile("r/src/temp.txt"))
    val input = new PagedSeqReader(PagedSeq.fromFile("r/src/r parsercheck.txt"))
    val tokens = new lexical.Scanner(input)
    val result = phrase(rProgram)(tokens)
    result match {
      case Success(tree, _) => println(tree)

      case e: NoSuccess => {
        Console.err.println(e)
        exit(-1)
      }
    }
  }

  def parseUnwrap(input: String): Any = {
    parseUnwrap(input, program)
  }

  def parseUnwrap(input: String, f: => Parser[Expression]): Any = {
    parse(input, f) match {
      case Success(tree, _) => tree
      case e: NoSuccess => {
        Console.err.println(e)
        e
      }
    }
  }

  def parse(input: String, f: => Parser[Expression]) = {
    val tokens = new lexical.Scanner(input)
    phrase(f)(tokens)
  }

}



