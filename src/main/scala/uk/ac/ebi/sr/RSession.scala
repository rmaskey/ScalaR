package uk.ac.ebi.sr

import collection.mutable.Map
import functions._
import model.{RObject, Environment}
import java.io.{FileFilter, File}
import rpackage.{PackageLoader, RPackage}

/**
 *
 * Date: Aug 11, 2010
 * @author Taalai Djumabaev
 */
class RSession {

  //todo for now only one session exists
  val global = RSession.globalEnv
  val base = RSession.baseEnv

  val loadedLibraries = Map[String, RPackage]()
  val packages = "packages/"
  val waitingPacks = collection.mutable.Set[File]() ++ (new File(packages)).listFiles(new FileFilter {
    def accept(f: File) = f.isDirectory
  })

  waitingPacks.foreach((f: File) =>
  if (!loadedLibraries.contains(f.getName)) { RPackage.loadPackage(f, base, this) })
}

object RSession {


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
  "library" -> PackageLoader)
  val globalEnv = Environment.childEnv(baseEnv)
  
  val currentSession = new RSession //todo to change
}