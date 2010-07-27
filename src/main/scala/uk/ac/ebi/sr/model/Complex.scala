package uk.ac.ebi.sr.model

/**
 *
 * Date: Jun 22, 2010
 * @author Taalai Djumabaev
 */
class Complex(val r: Double, val i: Double = 0.) {
  
  def + (that: Complex) = new Complex(r + that.r, i + that.i)
  def - (that: Complex) = new Complex(r - that.r, i - that.i)

  def * (that: Complex) = new Complex(r - that.r, i - that.i) // todo
  def / (that: Complex) = new Complex(r - that.r, i - that.i) // todo

  def unary_- = new Complex(-r, -i)

  override def toString = "" + r + (if (i >= 0) "+" + i else i)  + "i"
  def isZero = r == 0 && i == 0
}

object Complex {
  implicit def double2Complex(d: Double): Complex = new Complex(d, 0)
  implicit def int2Complex(i: Int): Complex = new Complex(i, 0)
  implicit def complex2String(i: Complex): String = i.toString
}

object ZeroComplex extends Complex(0, 0)