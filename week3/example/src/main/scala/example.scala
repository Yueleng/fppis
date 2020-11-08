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
  // CONSTRUCTOR OVERLOADING
  def this(x: Int) = this(x, 1) // alternative constructor

  // Start of PRIMARY CONSTRUCTOR
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
//  def numer: Int = x / g
//  def denom: Int = y / g

  // only computed once.
  val numer: Int = x / g
  val denom: Int = y / g
  // end of PRIMARY CONSTRUCTOR

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


// week 5
def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum(ys)
}


def prod(xs: List[Int]): Int = xs match {
  case Nil => 1
  case y :: ys => y * prod(ys)
}

def sum2(xs: List[Int]) = (0 :: xs) reduceLeft((x,y) => x + y)
def prod2(xs: List[Int]) = (1 :: xs) reduceLeft((x,y) => x * y)

def sum3(xs: List[Int]) = (xs foldLeft 0)(_ + _)
def prod3(xs: List[Int]) = (xs foldLeft 1)(_ * _)

abstract class List1[T] {
  def reduceLeft(op: (T, T) => T): T = this match {
    case Nil => throw new Error("Nil.reduceLeft")
    case x :: xs => (xs foldLeft x)(op)
  }

  def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
    case Nil => z
    case x :: xs => (xs foldLeft op(z, x))(op)
  }

  def reduceRight(op: (T, T) => T): T = this match {
    case Nil => throw new Error("Nil.reduceRight")
    case x :: Nil => x
    case x :: xs => op(x, xs.reduceRight(op))
  }

  def foldRight[U](z: U)(op: (T,U) => U): U = this match {
    case Nil => z
    case x :: xs => op(x, (xs foldRight z)(op))
  }
}

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys) (_ :: _) // foldLeft not hold here: since op(z,x) means ::(ys, x) which does not hold.