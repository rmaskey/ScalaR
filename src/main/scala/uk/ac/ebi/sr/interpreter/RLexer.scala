package uk.ac.ebi.sr
package interpreter

import util.parsing.syntax.StdTokens
import collection.mutable.HashSet
import util.parsing.combinator.RegexParsers
import util.parsing.combinator.lexical.Lexical

/**
 * Tokens of the parser 
 *
 * Date: 14.03.2010
 * @author Taalai Djumabaev
 */
trait RTokens extends StdTokens {

  case class DecimalNum(chars: String) extends Token {
    override def toString = chars
  }

  case class ComplexNum(chars: String) extends Token {
    override def toString = chars + "i"
  }

  // class for user-defined operation like %xyz%
  case class UserDefinedOperation(chars: String) extends Token {
    override def toString = "%" + chars + "%"
  }

  case class LitIdentifier(chars: String) extends Token {
    override def toString = chars
  }

  case object NewLineDelimiter extends Token {
    def chars = "\n"
    //def chars = "\r\n"
    override def toString = chars
  }
}

class RLexer extends Lexical with RTokens with RegexParsers {
  override type Elem = Char
  import scala.util.parsing.input.CharArrayReader.EofCh

  def token: Parser[Token] =
    ( '\012'                    ^^^ NewLineDelimiter
    //( '\015' ~ '\012'                    ^^^ NewLineDelimiter       // todo OS differences
    | hexNum                             ^^ { case hexNum(h) => NumericLit(Integer.parseInt(h, 16) toString) }
    | decimal ^^ { case decimal(null, _, _, _, d2, m2, e2, i2) =>  createNumberToken(d2, m2, e2, i2)
                   case decimal(d1, m1, e1, i1, _, _, _, _) => createNumberToken(d1, m1, e1, i1) }
    | letter ~ ((letter| '_' | digit)*)  ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | string ^^ LitIdentifier
    | stringD ^^ LitIdentifier
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | '%' ~> (chrExcept(' ', '\n', EofCh, '%')*) <~ '%' ^^ { case chars => UserDefinedOperation(chars mkString "")}
    | delim
            | EofCh ^^^ EOF
    | failure("illegal character")
    )

  override def letter = elem("letter or dot", ch => ch.isLetter || ch == '.')
  //tab is not supported in R? at lest r-gui doesn't admit it
  override def whitespaceChar = elem("space char", ch => (ch == ' ' || ch == '\t') && ch != EofCh)
  override def whitespace: Parser[Any] = rep(whitespaceChar | '#' ~ rep(chrExcept(EofCh, '\n', '\r'))) //^^ {case s => println(s);s }

  def string = '\'' ~> rep(charSeq | chrExcept('\'', '\n', EofCh)) <~ '\'' ^^ { _ mkString "" }
  def stringD = '\"' ~> rep(charSeq | chrExcept('\"', '\n', EofCh)) <~ '\"' ^^ { _ mkString "" }
  def charSeq: Parser[String] =
    ('\\' ~ '\"' ^^^ "\""
    |'\\' ~ '\\' ^^^ "\\"
    |'\\' ~ '/'  ^^^ "/"
    |'\\' ~ 'b'  ^^^ "\b"
    |'\\' ~ 'f'  ^^^ "\f"
    |'\\' ~ 'n'  ^^^ "\n"
    |'\\' ~ 'r'  ^^^ "\r"
    |'\\' ~ 't'  ^^^ "\t")
  
  //todo reduce duplication
  val decimal = """(\d+)(\.\d*)?([eE][+-]?\d+)?(i)?|(\d*)(\.\d+)([eE][+-]?\d+)?(i)?""" r
  val hexNum  = """0x([a-fA-F0-9]+)""" r

  val reserved = new HashSet[String]
  val delimiters = new HashSet[String]

  protected def processIdent(name: String) =
    if (reserved contains name) Keyword(name) else Identifier(name)

  private var _delim: Parser[Token] = null
  protected def delim: Parser[Token] = {
    if (_delim eq null) {
      def parseDelim(s: String): Parser[Token] = accept(s.toList) ^^ { x => Keyword(s) }

      val d = new Array[String](delimiters.size)
      delimiters.copyToArray(d,0)
      scala.util.Sorting.quickSort(d)
      _delim = d.toList.reverse.map(parseDelim).reduceRight[Parser[Token]](_ | _)
    }
    _delim
  }

  protected def createNumberToken(d: String, m: String, e: String, i: String) = (m, e, i) match {
      case (null, null, null) => NumericLit(d)
      case (null, null, _) => ComplexNum(d)

      case (mant, null, null) => DecimalNum(d + mant)
      case (mant, null, _) => ComplexNum(d + mant)

      case (null, float, null) => DecimalNum(d + float)
      case (null, float, _) => ComplexNum(d + float)

      case (mant, float, null) => DecimalNum(d + mant + float)
      case (mant, float, _) => ComplexNum(d + mant + float)
  }
}
