package uk.ac.ebi.sr
package rpackage

import interpreter.{Interpreter, RParser}
import model.{RObject, Environment}
import collection.mutable.Map
import java.io.{FileInputStream, FilenameFilter, File}
import java.util.jar.JarInputStream
import java.net.{URLClassLoader, URL}

/**
 *
 * Date: Aug 11, 2010
 * @author Taalai Djumabaev
 */

class RPackage(val name: String, val env: Environment, val exported: Map[String, RObject])

object RPackage {

  val R_SOURCE = "/R"
  val JAR_SOURCE = "/JARS"
  val R_EXTENSION = ".R"
  val JAR_EXTENSION = ".JAR"
  val NAMESPACE = "/NAMESPACE"

  def loadPackage(dir: File, baseEnv: Environment, session: RSession): RPackage = {
    val env = loadJars(new File(dir + JAR_SOURCE))
    val packageEnv = Environment.childEnv(baseEnv)
    val exportList: Option[List[String]] = readNamespace(new File(dir + NAMESPACE), packageEnv, session)
    loadR(new File(dir + R_SOURCE), packageEnv)
    val pack = exportList match {
      case Some(l) => new RPackage(dir.getName, packageEnv, createExportMap(l, packageEnv) ++ env)
      case None => new RPackage(dir.getName, packageEnv, packageEnv.ids ++ env)
    }
    session.loadedLibraries += dir.getName -> pack
    session.global.addPackage(dir.getName, pack.exported)  //todo maybe not global, but the calling env
    pack
  }

  def readNamespace(namespaceFile: File, packageEnv: Environment, session: RSession) =
    if (!namespaceFile.exists || namespaceFile.isDirectory) None
    else {
      val parsedNamespace = (new NamespaceParser).parse(namespaceFile.getAbsolutePath)
      parsedNamespace._1.foreach((s: String) =>
        if (!session.loadedLibraries.contains(s)) session.waitingPacks.find(_.getName == s) match {
          case Some(f) => {
            val pack = loadPackage(f, packageEnv.parent.get, session) //since we know, that parent exists and it's a baseEnv
            packageEnv.addPackage(s, pack.exported)
          }
          case None => error("required package wasn't found: " + s)
      } else packageEnv.addPackage(s, session.loadedLibraries.get(s).get.exported))
      Some(parsedNamespace._2)
    }


  def loadR(rDir: File, packageEnv: Environment) = if (rDir.exists && rDir.isDirectory) {
    val rFiles = rDir.list(new FilenameFilter {
      def accept(dir: File, name: String) = name.toUpperCase.endsWith(R_EXTENSION)
    })
    val dirPath = rDir.getAbsolutePath + "/"
    rFiles.foreach((file: String) => Interpreter.interpret(RParser.parseUnwrapFromFile(dirPath + file), packageEnv))
  }

  def loadJars(jarDir: File) = if (!jarDir.exists || !jarDir.isDirectory) Map[String, RObject]() else {
    val jarFiles = jarDir.listFiles(new FilenameFilter {
      def accept(dir: File, name: String) = name.toUpperCase.endsWith(JAR_EXTENSION)
    })
    val jEnv = Map[String, RObject]()
    for (n: File <- jarFiles) {
      val exp = exportFromJar(n)
      jEnv ++= exp
    }
    jEnv
  }

  def exportFromJar(jarFile: File) = {
    val jin = new JarInputStream(new FileInputStream(jarFile))
    val mainClass = jin.getManifest.getMainAttributes.getValue(java.util.jar.Attributes.Name.MAIN_CLASS);
    val loader = new JarLoader(new Array[URL](0))
    loader.addFile(jarFile.getAbsolutePath)

    val clazz = Class.forName(mainClass, true, loader)
    val exporter: PackageExporter = clazz.newInstance.asInstanceOf[PackageExporter]
    exporter.exportS
  }

  def createExportMap(exportList: List[String], env: Environment) = Map[String, RObject]() ++
    (for (ex <- exportList) yield env.resolveLocal(ex) match {
      case Some(o) => (ex, o)
      case None => error(ex + " not found in the loadnig library: " + ex)
    })

  class JarLoader(urls: Array[URL]) extends URLClassLoader(urls) {

    val JAR_PREFIX = "jar:file://"
    val JAR_SUFFIX = "!/"

    def addFile(path: String) = {
      val urlPath = JAR_PREFIX + path + JAR_SUFFIX;
      addURL(new URL(urlPath));
    }
  }
}





