import scala.annotation.tailrec
import scala.collection.IterableOnce.iterableOnceExtensionMethods
import scala.io.Source
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


// week6 homework
type Word = String

type Occurrences = List[(Char, Int)]

//def wordOccurrences(w: Word) = w.toLowerCase.toList(_)
//
//wordOccurrences("STRING")

val fruit = List("apple", "pear", "orange", "pineapple")

fruit groupBy (_.head)
// println(fruit groupBy (_.head))

val w = "Stringss"
// ((w.toLowerCase.toList groupBy (c => c)).toList).map((character, listOfC) => (character, listOfC.length))

def combinations(occurrences: Occurrences): List[Occurrences] = {
  @tailrec
  def combineIter(occurrences: Occurrences, acc: List[Occurrences]): List[Occurrences] = {
    if (occurrences.isEmpty) acc
    else {
      val charOcc = occurrences.head
      val c = charOcc._1
      val nextIterAcc = (for {
        i <- 0 to charOcc._2
        charsOcc <- if (acc.isEmpty) List(List()) else acc
      } yield ((c, i) :: charsOcc) sortBy (_._1) ).toList
      combineIter(occurrences.tail, nextIterAcc)
    }
  }
  combineIter(occurrences, List()) map (occ => occ filter (charOcc => charOcc._2 > 0))
}

combinations(List(('o', 1), ('k', 1)))

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  @tailrec
  def getCount(occ: Occurrences, c: Char): Int = {
    if (occ.isEmpty) 0
    else if (occ.head._1 == c) occ.head._2
    else getCount(occ.tail, c)
  }

  def adjust(pair: (Char, Int)): (Char, Int) = {
    val (c, count) = pair
    println(c + ": " + count)
    println(c + ": " +  getCount(x, c))
    c -> (getCount(x, c) - count)
  }

  def deductTerm(terms0: Map[Char, Int], term: (Char, Int)): Map[Char, Int] = {
    // term is from x, terms is y, weird!!
    val terms = terms0 withDefaultValue 0 // immutability, thus we have to create a new var
    val (c, count) = term
    terms + (c -> (-terms(c) + count)) // Map + (Key -> Value): Means update or add new key value pair.
  }

  if (y.isEmpty) x filter (pair => pair._2 > 0)
  else (x.toMap foldLeft y.toMap)(deductTerm).toList filter (pair => pair._2 > 0)

}

subtract(List(('a', 2), ('b', 2)), List(('a', 1), ('b', 0)))
subtract(List(('a', 2), ('b', 2)), List())
subtract(List(('a', 1), ('s', 4), ('e', 2), ('m', 1), ('n', 1), ('t', 1)), List(('a', 1), ('s', 4), ('e', 1)))

// Helper Function
def count(occ: List[(Char, List[Char])]): Occurrences = {
  def countAcc(occ: List[(Char, List[Char])], acc: List[(Char, Int)]): Occurrences = {
    if (occ.isEmpty) acc
    else countAcc(occ.tail, (occ.head._1, occ.head._2.size) :: acc)
  }
  countAcc(occ, List())
}

def wordOccurrences(w: Word): Occurrences = {
  count((w.toLowerCase.toList groupBy (c=>c)).toList)
  // map((character: Char, listOfC: List[Char]): (Char, Int) => (character, listOfC.size))
}

wordOccurrences("Heath")
("Heath".toLowerCase.toList groupBy (c=>c))