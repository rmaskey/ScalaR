package uk.ac.ebi.sr.model

object Type extends Enumeration {  
  type Type = Value

  val NULL = Value("NULL")
  val SYMBOL = Value("symbol")
  val PAIRLIST = Value("pairlist")
  val CLOSURE = Value("closure")
  val ENVIRONMENT = Value("environment")
  val PROMISE = Value("promise")
  val LANGUAGE = Value("language")
  val SPECIAL = Value("special")
  val BUILTIN = Value("builtin")
  val CHAR = Value("char")
  val LOGICAL = Value("logical")
  val INTEGER = Value("integer")
  val DOUBLE = Value("double")
  val COMPLEX = Value("complex")
  val CHARACTER = Value("character")
  val DOTDOTDOT = Value("dotdotdot")
  val ANY = Value("any")
  val EXPRESSION = Value("expression")
  val LIST = Value("list")
  val BYTECODE = Value("bytecode")
  val EXTERNALPTR = Value("externalptr")
  val WEAKREF = Value("weakref")
  val RAW = Value("raw")
  val S4 = Value("s4")
}