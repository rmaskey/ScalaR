package uk.ac.ebi.sr.rpackage

import uk.ac.ebi.sr.functions.Builtin
import uk.ac.ebi.sr.model.{RObject, Environment}
import uk.ac.ebi.sr.RSession
import uk.ac.ebi.sr.model.RVal.RChar

import java.io.File
import uk.ac.ebi.sr.interpreter.{DeclArg, FDeclArg, NULL}
import swing._

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

  def loadPackage(r: String) = {
    //todo don't need since for now we just load all the libraries
    //val loaded = RSession.currentSession.loadedLibraries
    // if (loaded.contains(r)) RSession.global.addPackage(r, loaded.get(r).get)

    val chooser = new FileChooser
    chooser.multiSelectionEnabled_=(true)
    chooser.fileSelectionMode_=(FileChooser.SelectionMode.DirectoriesOnly)
    //todo for now Grid is used
    chooser.showDialog(new GridPanel(1,1), "select")
    val seq = chooser.selectedFiles
    for (f: File <- seq) {
      if (f.getName != r) {} //todo warning should be herreor error?
      RPackage.loadPackage(f, RSession.currentSession.base, RSession.currentSession)
    }
    NULL //todo print will be RObject later (print(null) - should print nothing)
  }
}