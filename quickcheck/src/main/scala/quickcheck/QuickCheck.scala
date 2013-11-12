package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("minimum of two elements is smallest") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) ==  (a min b)
  }

  property("delete single elemnt from heap is empty") = forAll { (a: A) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("finding and deleting minima is ordered") = forAll { (h: H) =>
    isOrdered(popAll(h))
  }

  property("minimum of melded heaps should be minimum of both inputs") = forAll { (l: H, r: H) =>
    val melded = meld(l, r)
    val minimum = (findMin(l) min findMin(r))
    findMin(melded) == minimum
  }

  property("melded heap contains all elements of inputs") = forAll { (l: H, r: H) =>
    val expected = (popAll(l) ++ popAll(r)).sorted
    val actual = popAll(meld(l, r)).sorted
    expected == actual
  }

  def popAll(h: H): List[A] = {
    if (isEmpty(h)) List() else findMin(h) :: popAll(deleteMin(h))
  }

  def isOrdered(as: List[A]): Boolean = as match {
    case Nil => true
    case _ :: Nil => true
    case b :: bs => if (b <= bs.head) isOrdered(bs) else false
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
