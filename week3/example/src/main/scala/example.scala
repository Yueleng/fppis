object example extends App {
  val x = new Rational(1, 2)
  println(x.numer)
  println(x.denom)

//  def addRational(r: Rational, s: Rational): Rational =
//    new Rational(r.numer * s.denom + s.numer * r.denom, r.denom * s.denom)
//
//  def makeString(r: Rational) = r.numer + "/" + r.denom
//
//  println(makeString(addRational(new Rational(1,2), new Rational(2,3))))
  val y = new Rational(2, 3)
  println(x + y)

  val z = new Rational(3,2)
  println(x - y - z)
  println(y + y)
  println(x < y)
  println(x max y)
}


class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1) // alternative constructor

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
//  def numer: Int = x / g
//  def denom: Int = y / g

  // only computed once.
  val numer: Int = x / g
  val denom: Int = y / g

  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom, denom * that.denom
    )

  def < (that: Rational) = numer * that.denom < that.numer * denom

  def max(that: Rational) = if (this < that) that else this

  override def toString: String = numer + "/" + denom

  def unary_- : Rational = // spce between `unary_-` and ':'
    new Rational(-numer, denom)

  def - (that: Rational) = this + -that
}