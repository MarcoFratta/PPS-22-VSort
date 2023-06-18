package model

import model.SortableFunctionalities.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SortableMTest extends AnyFlatSpec with Matchers:

  import SortOperation.*

  given comp:Comparable[Int] with
      override def compare(a: Int, b: Int): Boolean = a - b > 0

  "A sortable with steps" should "exists" in {
    SortableM(Seq.empty, Seq.empty)
  }

  "A comparison " should "add one comparison steps" in {
    val s = SortableM(Seq(5, 6, 7), Seq.empty)
    for c <- s.compare(0, 1)(x => x)(x => x)
        comp2 <- c.compare(0,2)(x => x)(x => x)
        comp3 <- comp2.swap(1,0)
          yield comp3.data shouldEqual Seq(6,5,7)
}