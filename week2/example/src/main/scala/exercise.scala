import scala.annotation.tailrec

object exercise extends App{
//  def product(f: Int => Int)(a: Int, b: Int): Int = {
//    if (a > b) 1
//    else f(a) * product(f)(a+1, b)
//  }

  def product(f: Int => Int)(a: Int, b:Int): Int = mapReduce(f, (x,y)=> x*y, 1)(a,b)
  println(product(x => x * x)(3,4))

  def fact(n: Int) = product(x => x)(1, n)
  println(fact(5))

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1,b))
  }

  def abs(d: Double): Double = {
    if (d < 0) -d else d
  }

  var tolerance = 0.0001
  def isCloseEnough(x: Double, y: Double) = abs((x-y)/x) < tolerance

  @tailrec
  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
//    def iterable(guess: Double): Double = {
//      val next = f(guess)
//      if (isCloseEnough(guess, next)) next
//      else iterable(next)
//    }
//    iterable(firstGuess)
    val next = f(firstGuess)
    if (isCloseEnough(firstGuess, next)) next
    else fixedPoint(f)(next)
  }

  println(fixedPoint(x=>x/2+1)(1))

//  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1.0)
//  println(sqrt(2))

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double): Double = fixedPoint( y => averageDamp(p => (p + x / p)/2)(y) )(1.0)
  println(sqrt(2))

  def sqrt2(x: Double): Double = fixedPoint( averageDamp(y => x/y) )(1.0)
}
