package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import CreateHeap.*
class HeapifyTest extends AnyFlatSpec with Matchers:

  "A sequence (4,10,3,5,1) " should "be (10, 5, 3, 4, 1) after heapify" in {
    val seq = heapify(Seq(4,10,3,5,1))
    assert(seq == Seq(10, 5, 3, 4, 1))
  }

  "A sequence (7, 6, 5, 4, 3, 2, 1) " should "be (7, 6, 5, 4, 3, 2, 1) after heapify" in {
    val seq = heapify(Seq(7, 6, 5, 4, 3, 2, 1))
    assert(seq == Seq(7, 6, 5, 4, 3, 2, 1))
  }

  "A sequence (5, 4, 3, 2, 1) " should "be (5, 4, 3, 2, 1) after heapify" in {
    val seq = heapify(Seq(5, 4, 3, 2, 1))
    assert(seq == Seq(5, 4, 3, 2, 1))
  }

  "A sequence (2, 8, 5, 3, 9, 1, 6) " should "be (9, 8, 6, 3, 2, 1, 5) after heapify" in {
    val seq = heapify(Seq(2, 8, 5, 3, 9, 1, 6))
    assert(seq == Seq(9, 8, 6, 2, 3, 1, 5))
  }