package uk.ac.ebi.sr.rpackage

import collection.JavaConversions._
import uk.ac.ebi.sr.model.RObject

/**
 * A trait used for loading packages by jar files.
 * Packages should contain a class mixing this trait and it shoulb written as Main-class
 * in Manifest 
 *
 * Date: Aug 13, 2010
 * @author Taalai Djumabaev
 */
trait PackageExporter {

  /**
   * Method that is used for export
   */
  def exportS: collection.mutable.Map[String, RObject] = exportJ

  /**
   * Method for implementation from Java language. The resulting collection will be converted
   * to scala.collection.mutable.Map and returned by exportS method
   */
  def exportJ: java.util.Map[String, RObject] = error("unimplemented method for export")
}

object PackageExporter