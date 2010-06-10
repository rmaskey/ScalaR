package uk.ac.ebi.jr.model

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */

abstract class RObject {

  val objectType:Type.Type

  lazy val length = 0

}