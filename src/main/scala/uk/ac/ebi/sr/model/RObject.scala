package uk.ac.ebi.sr.model

/**
 *
 * Date: 20.05.2010
 * @author Taalai Djumabaev
 */

abstract class RObject {

  val objectType:Type.Type

  lazy val length = 0

}