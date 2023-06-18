package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IterableSortableTest extends AnyFlatSpec with Matchers:

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0


  "A sortable " should "be used with for comprehension" in {
    assert ((for i <- IterableS(1,2,3) yield  1) == 3)
  }