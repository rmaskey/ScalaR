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
package gui

import interpreter._
import model.Environment

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */
class RExecutor(var env: Environment) {
  def parseAndFormat(input: String): (String, String) = {
    val result = new StringBuilder()
    val tree = new StringBuilder()

    RParser.parseUnwrap(input, RParser.rProgram) match {
      case r: RParser.Failure => {
        result.append(r.toString)
        result.append("\n")
      }
      case e: Expression => {
        result.append("> ")
        result.append(input)
        result.append("\n")
        tree.append(e.toString)
        try {
          val interpretation = Interpreter.interpret(e, env)
          env = interpretation._2
          result.append(interpretation._1)
        } catch {
          case e: java.lang.RuntimeException => {
            result.append("error: ")
            result.append(e.getMessage())
            e.printStackTrace
          }
        } finally {
          result.append("\n")
          tree.append("\n")
        }
      }
      case er => println("unexpected error " + er)
    }
    return (result.toString, tree.toString)
  }

  def interpret(input: String): String = RParser.parseUnwrap(input, RParser.rProgram) match {
    case r: RParser.Failure => r.toString
    case Block(List(e: NoPrintedReturnExpression)) => try {
        Interpreter.interpret(e, env)._1
        RExecutor.EMPTY_RES
      } catch {
        case ex: java.lang.RuntimeException => "error: " + ex.getMessage()
      }
    case e: Expression =>
      try {
        Interpreter.interpret(e, env)._1.toString
      } catch {
        case ex: java.lang.RuntimeException => "error: " + ex.getMessage()
      }
    case er => "unexpected error: " + er
  }
}

object RExecutor {
  val EMPTY_RES = ""
}