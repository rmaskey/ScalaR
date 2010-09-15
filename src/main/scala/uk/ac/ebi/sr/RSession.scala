package uk.ac.ebi.sr

import collection.mutable.Map
import functions._
import model.Environment
import java.io.{FileFilter, File}
import rpackage.{Libs, PackageLoader, RPackage}

/**
 * Only one session per scalar is supported for now.
 * 
 * Date: Aug 11, 2010
 * @author Taalai Djumabaev
 */
class RSession {

  //todo for now only one session exists
  val global = RSession.globalEnv
  val base = RSession.baseEnv

  val loadedLibraries = Map[String, RPackage]()
  val waitingPacks = {
    val set = collection.mutable.Set[File]()
    RSession.libs.foreach((s: String) =>
      set ++ (new File(s)).listFiles(new FileFilter {
        def accept(f: File) = f.isDirectory
      }))
    set
  }

  waitingPacks.foreach((f: File) =>
    if (!loadedLibraries.contains(f.getName)) RPackage.loadPackage(f, base, this)
    else {}) //todo warning
}

object RSession {
  val packages = "packages/"
  val libs = scala.collection.mutable.ListBuffer[String](packages)

  val baseEnv = Environment.emptyEnv ++= Map(
    "attr" -> Attr,
    "attributes" -> Attributes,
    "length" -> Length,
    "as.integer" -> AsInteger,
    "as.logical" -> AsLogical,
    "as.double" -> AsDouble,
    "as.character" -> AsCharacter,
    "c" -> Concat,
    "typeof" -> TypeOf,
    "library" -> PackageLoader,
    "list" -> RLangList,
    "addLibDir" -> Libs,
    "library" -> PackageLoader,
    "sin" -> Sine)
  val globalEnv = Environment.childEnv(baseEnv)
  
  val currentSession = new RSession //todo to change
}