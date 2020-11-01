import scala.annotation.tailrec

"Hello World"

// Evalutaion Rules
// Call by value: evaluates the function arguments before calling the function
def example = 2 // evaluated when called
val example2 = 2 // evaluated immediately
// lazy val example3 = 2 // evaluated once when needed

// def square(x: Double) // call by value
// def square(x: => Double) // call by name





def sqrt(x: Double) = {
  @tailrec
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improves(guess, x), x)

  def abs(x: Double) = if (x < 0) -x else x

  def isGoodEnough(guess: Double, x: Double): Boolean =
    abs(guess * guess - x) / x < 0.00001

  def improves(guess: Double, x: Double): Double = (guess + x / guess) / 2

  sqrtIter(1.0, x)
}

def sqrtConcise(x: Double) = {
  @tailrec
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improves(guess))

  def abs(x: Double) = if (x < 0) -x else x

  def isGoodEnough(guess: Double): Boolean =
    abs(guess * guess - x) / x < 0.00001

  def improves(guess: Double): Double = (guess + x / guess) / 2



  sqrtIter(1.0)
}




sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)

sqrtConcise(2)

val x = 1
val y = x + 1; y*y


// Week3
def error(msg: String) = throw new Error(msg)
val nulll = null
val javaString: String = nulll

// null not subtype of scala.AnyVal.
// Instead null is subtype of scale.AnyRef(i.e. java.lang.Object)
// val z: Int = nulll

// returns AnyVal
if (true) 1 else false


