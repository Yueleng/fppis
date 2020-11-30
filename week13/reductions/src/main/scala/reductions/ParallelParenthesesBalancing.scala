package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceIter(cumulative: Int, chars: Array[Char]): Boolean = {
      if (cumulative < 0) false
      else if (chars.isEmpty) cumulative == 0
      else {
        if (chars.head == '(') balanceIter(cumulative + 1, chars.tail)
        else if (chars.head == ')') balanceIter(cumulative - 1, chars.tail)
        else balanceIter(cumulative, chars.tail)
      }
    }
    balanceIter(0, chars)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalanceAlt(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, leftPar: Int, rightPar: Int): Int = {
      if (idx == until) leftPar - rightPar
      else if (chars(idx) == '(') traverse(idx + 1, until, leftPar + 1, rightPar)
      else if (chars(idx) == ')') traverse(idx + 1, until, leftPar, rightPar + 1)
      else traverse(idx + 1, until, leftPar, rightPar)
    }

    def reduce(from: Int, until: Int): Int = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid = from + (until - from) / 2
        val (cum1, cum2) = parallel(reduce(from, mid), reduce(mid, until))
        cum1 + cum2
      }
    }

    if (chars.isEmpty) true
    else reduce(0, chars.length) == 0
  }

  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      // God I hate this imperative coding for parallelism's sake!
      var i = idx
      var begin = 0
      var end = 0

      while (i < until){
        if(chars(i) == '(')
          if (begin < 0) end = end + 1
          else if (begin >= 0) begin = begin + 1
          // if(switched) end = end + 1 else begin = begin + 1

        if(chars(i) == ')')
          if (begin < 0) end = end - 1
          else if (begin >= 0)  begin = begin - 1
          //if(switched) end = end - 1 else begin = begin - 1
        i = i + 1
      }

      (begin,  end)

    }

    /*
                             (1,-1)
                ((test)(c+x)-4)((((4)444)---)xx(x)x)
                               / \
                  (1,2)  =>         (-2, -1)
            ((test)(c+x)-4)(((    (4)444)---)xx(x)x)
       (2,0)    (-1,2)                (-1,0)   (-1, -1)
         /             \                /      \
    ((test)(c     +x)-4)((()(       (4)444)--      -)xx(x)x)
    */



    def reduce(from: Int, until: Int): (Int, Int) = {
      if(until - from <= threshold) traverse(from, until)
      else {
        val mid = from + (until - from) / 2
        val (pair1, pair2) = parallel(reduce(from, mid), reduce(mid, until))

        if (pair1._1 < 0 && pair2._1 > 0) (pair1._1 , pair2._1 + pair1._2 + pair2._2)
        else if(pair2._1 < 0 && pair1._2 > 0) (pair1._1 + pair2._1 + pair1._2 ,  + pair2._2)
        else (pair1._1 + pair2._1, pair1._2 + pair2._2)
      }
    }

    val res = reduce(0, chars.length)
    res._1 + res._2 == 0 && res._1 >= 0
  }



  // For those who want more:
  // Prove that your reduction operator is associative!

}
