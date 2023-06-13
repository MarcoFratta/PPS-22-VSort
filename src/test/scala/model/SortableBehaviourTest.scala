package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Failure, Success, Try}
import model.Sortable

class SortableBehaviourTest extends AnyFlatSpec with Matchers:
  import model.Sortable.*

  behavior of "A sortable"

  it should "exists" in {
    val mList = Sortable(0,1,2)
  }

  "After a swap, it" should "have a swap step and the elements must be swapped" in {
    val mList1 = Sortable(0, 1, 2)
    val mList2 = Sortable(List(1, 0, 2), List(Step.Swap(0,1)))
    mList1.swap(0,1) shouldBe Success(mList2)
  }

  "After a selection, it" should "have 1 selection step and the same data" in {
    val mList1 = Sortable(0, 1, 2)
    val mList2 = Sortable(List(0, 1, 2), List(Step.Selection(0)))
    mList1.select(0) shouldBe Success(mList2)
  }

  "After a deselection," should "have 1 deselection step and the same data" in {
    val mList1 = Sortable(0, 1, 2)
    val mList2 = Sortable(List(0, 1, 2), List(Step.Deselection(0)))
    mList1.deselect(0) shouldBe Success(mList2)
  }

  "An empty list" should "have size 0" in {
    Sortable().length() shouldBe 0
  }

  "A list with 2 elements" should "have size 2" in {
    Sortable(3,4).length() shouldBe 2
  }

  "After a comparison," should "have the same data and have a compare step" in {
    val mList1 = Sortable(1, 0, 2)
    val mList2 = Sortable(List(1, 0, 2), List(Step.Comparison(0, 1)))
    mList1.compare(0, 1)(x => x)(x => x)(using _ - _ > 0) shouldBe Success(mList2)
}