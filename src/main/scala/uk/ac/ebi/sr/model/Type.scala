package uk.ac.ebi.sr.model


/**
 * author: Nikolay Matveev
 */


object Type extends Enumeration {  
  type Type = Value

  val
  NULL,
  SYMBOL,
  PAIRLIST,
  CLOSURE,
  ENVIRONMENT,
  PROMISE,
  LANGUAGE,
  SPECIAL,
  BUILTIN,
  CHAR,
  LOGICAL,
  INTEGER,
  DOUBLE,
  COMPLEX,
  CHARACTER,
  DOTDOTDOT,
  ANY,
  EXPRESSION,
  LIST,
  BYTECODE,
  EXTERNALPTR,
  WEAKREF,
  RAW,
  S4
  = Value
}