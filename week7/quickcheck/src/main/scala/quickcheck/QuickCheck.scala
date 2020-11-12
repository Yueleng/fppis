package quickcheck

import java.lang.Math

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[A]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  lazy val genHeap2: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap2))
  } yield insert(n, h) // insert n into h

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll{a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("twoEle1") = forAll {(a: Int, b: Int) =>
    val h = empty
    findMin(insert(b, insert(a, h))) == Math.min(a,b)
  }

  //
  property("insertDelete1") = forAll { a: Int =>
    val h = empty
    isEmpty(deleteMin(insert(a, h))) == true
  }

  property("sortedSeq1") = forAll {h: H => {

    def deleteFromHeapIter(h: H, acc: List[Int]): List[Int] = {
      if (isEmpty(h)) acc
      else {
        val min = findMin(h)
        deleteFromHeapIter(deleteMin(h), acc ::: List(min))
      }
    }
    val popUpList = deleteFromHeapIter(h, Nil)
    popUpList == popUpList.sorted
  }}

  property("minOfTwoMelding1") = forAll {(h1: H, h2: H) => {
    val min1 = if (isEmpty(h1)) 0 else findMin(h1)
    val min2 = if (isEmpty(h2)) 0 else findMin(h2)

    val meldOfTwo = meld(h1, h2)
    val min = if (isEmpty(meldOfTwo)) 0 else findMin(meldOfTwo)
    min == Math.min(min1, min2)
  }}

  property("The minimal value of a melded Heap should be the min of the min of both heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }
}

