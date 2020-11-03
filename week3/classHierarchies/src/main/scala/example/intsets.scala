package example

import scala.annotation.tailrec

object intsets extends App {
  println("Welcome to the Scala worksheet")
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
  println(t1)
  println(t2)

  @tailrec
  def nth[T](n: Int, xs: List[T]): T = {
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n-1, xs.tail)
  }

  val list = new Cons(1, new Cons(2, new Cons(3, Nil)))


  println(nth(2, list))
  // println(nth(3, list))
  // println(nth(-1, list))

  // week 4
  val x: List[String] = Nil


  def show(e: Expr): String = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
    case Var(x) => x
    case Prod(x, y) => "(" + show(x) + ") * (" + show(y) + ")"
  }

  println(show(Sum(Number(1), Number(44))))

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

trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

// head and tail are already evaluated upon initialization.
class Cons[T] (val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

//class Nil[T] extends List[T] {
//  def isEmpty = true
//  def head = throw new NoSuchElementException("Nil.head")
//  def tail = throw new NoSuchElementException("Nil.tail")
//}

object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")
}


// Week 4
object List {
  // List(1, 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
  def apply[T](x1: T): List[T] = new Cons(x1, Nil)
  def apply[T](): List[Nothing] = Nil
}

trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(s: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr
