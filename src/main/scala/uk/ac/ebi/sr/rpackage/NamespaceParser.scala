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
package uk.ac.ebi.sr.rpackage

import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.lexical.StdLexical
import collection.mutable.ListBuffer
import util.parsing.input.PagedSeqReader
import collection.immutable.PagedSeq

/**
 * A package namespace file parser
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

/**
 * A package namespace file lexer
 */
class NamespaceLexer extends StdLexical {

  override def token =
    ( letter ~ ((letter| '_' | digit)*)  ^^
        { case first ~ rest => processIdent(first :: rest mkString "") }
    | delim
    | failure("Illegal character"))

  override def letter = elem("letter or dot", ch => ch.isLetter || ch == '.')
}