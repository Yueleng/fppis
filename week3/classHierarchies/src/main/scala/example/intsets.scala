package example

object intsets extends App {
  println("Welcome to the Scala worksheet")
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  println(t1)
  println(t2)

  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n-1, xs.tail)
  }

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))


  println(nth(2, list))
  println(nth(3, list))
  println(nth(-1, list))


}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

// OBJECT instead of CLASS means that it's singleton
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = ((left union right) union other) incl elem

  override def toString: String = "{" + left + elem + right + "}"


}

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

// head and tail are already evaluated upon initialization.
class Cons[T] (val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

class Nil[T] extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}
