import example.Pouring


def from(n: Int): LazyList[Int] = n #:: from(n+1)

val nats = from(0)
val m4s = nats map (_ * 4)

(m4s take 100).toList

def sieve(s: LazyList[Int]): LazyList[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))

val primes = sieve(from(2))

primes.take(100).toList

def sqrtStream(x: Double): LazyList[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: LazyList[Double] = 1 #:: (guesses map improve)
  guesses
}

def isGoodEnough(guess: Double, x: Double) =
  math.abs((guess * guess - x) / x) < 0.0001

(sqrtStream(20) take 20).toList

sqrtStream(20).filter(isGoodEnough(_, 20)).take(5).toList

// Pouring Water
val problem = Pouring
problem.moves