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

  property("minimum of two elements is smallest") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) ==  (a min b)
  }

  property("delete single elemnt from heap is empty") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  /*
  property("finding and deleting minima is ordered") = forAll { (h: H) =>

  }
  */

  property("minimum of melded heaps should be minimum of both inputs") = forAll { (l: H, r: H) =>
    val melded = meld(l, r)
    val minimum = (findMin(l) min findMin(r))
    findMin(melded) == minimum
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
