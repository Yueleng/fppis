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


// Week6
val xs = Array(1, 2, 3, 44)
xs map (x => x * 2)

val s = "Hello World"
s filter (c => c.isUpper)
s exists (c => c.isUpper)
s forall (c => c.isUpper)

val pairs = List(1, 2, 3) zip s
pairs.unzip

s flatMap (c => List('.', c)) // IndexedSeq instead of String


xs.sum
xs.max

def listCombine(M: Int, N: Int): IndexedSeq[(Int, Int)] = {
  (1 to M) flatMap (x => (1 to N) map (y => (x, y)))
}

listCombine(3, 5)

def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
  (xs zip ys).map {case (x,y) => x*y}.sum
}

def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)


val n = 7
(1 until n) map (i =>
  (1 until i) map (j => (i, j))) // vector of vectors. WHY?


((1 until n) map (i =>
  (1 until i) map (j => (i, j))) ).flatten

// We know that: xs flatMap f = (xs map f).flatten
// thus the above is equivalent to

(1 until n) flatMap (i => (1 until i) map (j => (i,j)))

(1 until n) flatMap (i => (1 until i) map (j => (i,j))) filter (pair =>
  isPrime(pair._1 + pair._2)
)

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProductFor(xs: List[Double], ys: List[Double]): Double =
  (for ((x,y) <- xs zip ys) yield x * y).sum