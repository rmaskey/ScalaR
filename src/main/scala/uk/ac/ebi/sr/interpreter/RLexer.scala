package uk.ac.ebi.sr
package interpreter

import util.parsing.syntax.StdTokens
import collection.mutable.HashSet
import util.parsing.combinator.RegexParsers
import util.parsing.combinator.lexical.Lexical

/**
 *
 * Date: 14.03.2010
 * @author Taalai Djumabaev
 */

trait RTokens extends StdTokens {

  case class DecimalNum(chars: String) extends Token {
    override def toString = chars
  }

  case class ComplexNum(format: Token) extends Token {
    def chars = format.chars
    override def toString = chars
  }

  // class for user-defined operation like %xyz%
  case class UserDefinedOperation(chars: String) extends Token {
    override def toString = "%" + chars + "%"
  }

  case class LitIdentifier(chars: String) extends Token {
    override def toString = chars
  }

  case object NewLineDelimiter extends Token {
    def chars = "\r\n"
    override def toString = chars
  }
}

class RLexer extends Lexical with RTokens with RegexParsers {
  override type Elem = Char
  import scala.util.parsing.input.CharArrayReader.EofCh

  def token: Parser[Token] =
    ( '\015' ~ '\012'                    ^^^ NewLineDelimiter
    | hexNum                             ^^ { case hexNum(h) => NumericLit(Integer.parseInt(h, 16) toString) }
    | decimal ^^ { case decimal(null, _, _, _, d2, m2, e2, i2) =>  createNumberToken(d2, m2, e2, i2)
                   case decimal(d1, m1, e1, i1, _, _, _, _) => createNumberToken(d1, m1, e1, i1) }
    | letter ~ ((letter| '_' | digit)*)  ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
    | '\'' ~> (chrExcept('\'', EofCh)*) <~ '\'' ^^ { case chars => LitIdentifier(chars mkString "") }
    | '\"' ~> (chrExcept('\"', EofCh)*) <~ '\"' ^^ { case chars => LitIdentifier(chars mkString "") }
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    | '%' ~> (chrExcept(' ', '\n', EofCh, '%')*) <~ '%' ^^ { case chars => UserDefinedOperation(chars mkString "")}
    | delim
    | failure("illegal character")
    )

  override def letter = elem("letter or dot", ch => ch.isLetter || ch == '.')
  //tab is not supported in R? at lest r-gui doesn't admit it
  override def whitespaceChar = elem("space char", ch => (ch == ' ' || ch == '\t') && ch != EofCh)
  override def whitespace: Parser[Any] = rep(whitespaceChar | '#' ~ rep(chrExcept(EofCh, '\n', '\r'))) //^^ { case c => println("white: " + c.size + ".." + c); c }

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

  protected def createNumberToken(d: String, m: String, e: String, i: String) = {
    //d parameter is not null according to regex of number. Others can be null
    val number = (d, m, e) match {
      case (s, null, null) => NumericLit(s)
      case (s, mant, null) => DecimalNum(s + mant)
      case (s, null, float) => DecimalNum(s + float)
      case (s, mant, float) => DecimalNum(s + mant + float)
    }
    if (i == null) number else ComplexNum(number)
  }
}
