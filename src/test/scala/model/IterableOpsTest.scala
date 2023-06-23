package model

import model.sortModel.*
import model.sortModel.SortAddOns.IterateOps
import model.sortModel.SortableFunctionalities.Comparable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IterableOpsTest extends AnyFlatSpec with Matchers:

  import model.sortModel.SortAddOns.IterateOps

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  "An iteration on (1,2,3)" should "not modify data" in {
    val s = SortableM(Seq(1, 2, 3))
    val y = (for i <- s.loopFor(0 to 3 by 1)
      yield i.previous).get.data
    y shouldEqual Seq(1, 2, 3)

  }
