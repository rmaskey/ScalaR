package uk.ac.ebi.sr
package gui

import interpreter._

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */

class RExecutor(var env: Environment) {

  def parseAndFormat(input: String): (String, String) = {
    val parser = if (input.contains(";")) RParser.program else RParser.expression
    val result = new StringBuilder()
    val tree   = new StringBuilder()

    RParser.parseUnwrap(input, parser) match {
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
}