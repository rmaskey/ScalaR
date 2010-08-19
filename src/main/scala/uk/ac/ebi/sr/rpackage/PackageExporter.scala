package uk.ac.ebi.sr.rpackage

import collection.JavaConversions._
import uk.ac.ebi.sr.model.RObject

/**
 *
 * Date: Aug 13, 2010
 * @author Taalai Djumabaev
 */
trait PackageExporter {

  def exportS: collection.mutable.Map[String, RObject] = exportJ
  def exportJ: java.util.Map[String, RObject]
}

object PackageExporter