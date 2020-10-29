package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println("Parentheses Balancing")
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println("Counting Change")
    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else {
      if (r == c) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceIter(left: Int, restChars: List[Char]): Boolean = {
      if (left < 0) false
      else if (restChars.isEmpty) left == 0
      else {
        if (restChars.head == '(')
          balanceIter(left + 1, restChars.tail)
        else if (restChars.head == ')')
          balanceIter(left - 1, restChars.tail)
        else
          balanceIter(left, restChars.tail)
      }
    }

    balanceIter(0, chars)
  }

  /**
   * Exercise 3
   */
  // coins = List(1,2,5) or coins = List(1,2) or ...
  def countChange(money: Int, coins: List[Int]): Int = {
    // @tailrec
    def countChangeIter(moneyLeft: Int, restCoins: List[Int]): Int = {
      if (moneyLeft < 0 || restCoins.isEmpty) 0
      else if (moneyLeft == 0) 1
      else countChangeIter(moneyLeft, restCoins.tail) + countChangeIter(moneyLeft - restCoins.head, restCoins)
    }
    countChangeIter(money, coins)
  }
}