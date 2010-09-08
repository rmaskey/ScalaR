package uk.ac.ebi.sr.rpackage

import uk.ac.ebi.sr.functions.Builtin
import uk.ac.ebi.sr.model.{RObject, Environment}
import uk.ac.ebi.sr.RSession
import uk.ac.ebi.sr.model.RVal.RChar

import uk.ac.ebi.sr.interpreter.{DeclArg, FDeclArg, NULL}
import swing._
import java.io.{FileFilter, File}

/**
 *
 * Date: Aug 16, 2010
 * @author Taalai Djumabaev
 */

object PackageLoader extends Builtin {

  val frame = new Frame
  val PACKAGE = "package"
  val params = List[FDeclArg](DeclArg(PACKAGE))

  protected def apply(env: Environment): RObject = env.resolve(PACKAGE) match {
    case Some(r: RChar) => if (r.length == 1) loadPackage(r.s(0)) else NULL
    case _ => NULL
  }

  def loadPackage(r: String): RObject = {
    val session = RSession.currentSession
    val loaded = session.loadedLibraries
    if (!loaded.contains(r)) {
      RSession.libs.foreach((s: String) => {
        val files = (new File(s)).listFiles(new FileFilter {
          def accept(f: File) = f.isDirectory
        })
        files.foreach((f: File) =>
          if (f.getName == r) { RPackage.loadPackage(f, session.base, session); return NULL })
      })
    }
    //todo warning here: package 'r' not found
    NULL //todo print will be RObject later (print(null) - should print nothing)
  }
}