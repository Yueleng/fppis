package example

import scala.io.Source

object Example extends App {
  println("Hello World")

  def abs(x: Double) = if (x < 0) -x else x;

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improves(guess, x), x)

  def isGoodEnough(guess: Double, x: Double): Boolean =
    abs(guess * guess - x) / x < 0.001

  def improves(guess: Double, x: Double): Double = (guess + x / guess) / 2

  def sqrt(x: Double) = sqrtIter(1.0, x)

  println(sqrt(2))

  // week6
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k: Int): Set[List[Int]] =
      if (k == 0) Set(List())
      else
        for {
          queens <- placeQueens(k-1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col ::  queens
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1) zip queens
      queensWithRow forall {
        case (r, c) => col != c && math.abs(col - c) != row - r
      }
    }
    placeQueens(n)
  }

  def show(queens: List[Int]) = {
    val lines =
      for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
    "\n" + (lines mkString "\n")
  }
  println(queens(4) map show)
  println(queens(8) take 3 map show mkString "\n")

  // Class Map[Key, Value]
  val romanNumberals = Map("I" -> 1, "V" -> 5, "X" -> 10)
  val capitalOfCountry = Map("US"->"Washington", "Switzerland"->"Bern")

  println(capitalOfCountry get "US")

  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "Missing Data"
  }

  println(showCapital("US"))
  println(showCapital("Andorra"))

  val fruit = List("apple", "pear", "orange", "pineapple")
  println(fruit.sortWith (_.length < _.length))
  println(fruit.sorted)

  println(fruit groupBy (_.head)) //> HashMap(p -> List(pear, pineapple),
                                  // |  a -> List(apple),
                                  // |  o -> List(orange))

  // Polynomial
  // x^3 - 2x + 5
  // Map(0 -> 5, 1 -> -2, 3 -> 1)
  class Poly(terms0: Map[Int, Double]) {
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
//    def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
    def + (other: Poly) = new Poly((other.terms foldLeft terms)(addTerm)) // foldLeft is more efficient
    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
//      terms get exp match {
//        case Some(coeff1) => exp -> (coeff + coeff1)
//        case None => exp -> coeff
//      }
      exp -> (coeff + terms(exp))
    }
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      // Think of foldLeft as List op Ele, thus :: is not suitable for foldLeft
      val (exp, coeff) = term
      terms + (exp -> (coeff + terms(exp)))

    }
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString " + "
  }

  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))

  println(p1 + p2)
  p1.terms(7)

  // Putting pieces together
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords")
  val words = in.getLines.toList filter (word => word forall ( char => char.isLetter))

  val mnem = Map(
    '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
    '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
  )

  /** Invert the mnem map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit
  println(charCode)

  /** Maps a word to the digit string it can represent, e.g. "Java" -> "5282" */
  def wordCode(word: String): String = {
    // (word.toUpperCase.toCharArray flatMap mnem).toString
    word.toUpperCase map charCode
  }

  println(wordCode("Java"))

  /*
  * A map from digit strings to the words that represent them,
  * e,g. "5282" -> List("Java", "Kata", "Lava", ...)
  * Note: A missing number should map to the empty set, e.g. "1111" -> List()
  */
  val wordsForNum: Map[String, Seq[String]] = {
    words groupBy wordCode withDefaultValue Seq()
  }
  print(wordsForNum)

  /** Return all ways to encode a number as a list of words*/
  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest // returns range or indexed sequence
    }.toSet

  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")

  translate("7225247386")


  // week 7
  trait Generator[+T] {
    self =>

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S] (f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(generate).generate
    }

    def choose(lo: Int, hi: Int): Generator[Int] =
      for (x <- integers) yield lo + x % (hi - lo)

    def oneOf[T](xs: T*): Generator[T] =
      for (idx <- choose(0, xs.length)) yield xs(idx)
  }

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  def single[T](x: T) = new Generator[T] {
    def generate = x
  }

  // The booleans Generator
  val booleans = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T,U)] {
    def generate: (T, U) = (t.generate, u.generate)
  }

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

  // val booleans = integers.map(_ > 0)
  // A `List` Generator

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def test[T](r: Generator[T], noTimes: Int = 100)(test: T => Boolean) {
    for (_ <- 0 until noTimes) {
      val value = r.generate
      assert(test(value), "Test failed for: " + value)
    }
    println("Text passed " + noTimes + " times")
  }

  test(pairs(lists, lists)) {
    case(xs, ys) => (xs ++ ys).length > xs.length
  }

  // A `Tree` generator
  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def leafs: Generator[Leaf] = for {
    x <- integers
  } yield Leaf(x)

  def inners: Generator[Inner] = for {
    l <- trees
    r <- trees
  } yield Inner(l, r)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  trees.generate
}

