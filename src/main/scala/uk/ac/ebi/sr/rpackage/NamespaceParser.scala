package uk.ac.ebi.sr.rpackage

import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.lexical.StdLexical
import collection.mutable.ListBuffer
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq

/**
 *
 * Date: Aug 12, 2010
 * @author Taalai Djumabaev
 */

class NamespaceParser extends StandardTokenParsers {
  override val lexical = new NamespaceLexer
  lexical.delimiters += ("(", ")", ",", "\"", "\'")
  lexical.reserved += ("import", "export")

  def namespace(im: ListBuffer[String], ex: ListBuffer[String]) =
    rep1( importList ^^ { case il => im ++= il }
        | exportList ^^ { case el => ex ++= el } )

  def importList = "import" ~> "(" ~> repsep(ident | quoted, ",") <~ ")"
  def exportList = "export" ~> "(" ~> repsep(ident | quoted, ",") <~ ")"

  def quoted = ("\"" | "\'") ~> ident <~ ("\"" | "\'")

  def parse(file: String) = {
    val im = ListBuffer[String]()
    val ex = ListBuffer[String]()
    val input = new PagedSeqReader(PagedSeq.fromFile(file))
    val tokens = new lexical.Scanner(input)
    phrase(namespace(im, ex))(tokens)
    (im.toList, ex.toList)
  }
}

class NamespaceLexer extends StdLexical {

  override def token =
    ( letter ~ ((letter| '_' | digit)*)  ^^
        { case first ~ rest => processIdent(first :: rest mkString "") }
    | delim
    | failure("Illegal character"))

  override def letter = elem("letter or dot", ch => ch.isLetter || ch == '.')
}