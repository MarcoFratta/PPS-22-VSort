package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Failure, Success, Try}

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

  "An empty list" should "have size 0" in {
    Sortable().length() shouldBe 0
  }

  "A list with 2 elements" should "have size 2" in {
    Sortable(3,4).length() shouldBe 2
  }

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  "After a comparison," should "have the same data and have a compare step" in {
    val mList1 = Sortable(1, 0, 2)
    val mList2 = Sortable(List(1, 0, 2), List(Step.Comparison(0, 1)))
    mList1.compare(0, 1)(x => x)(x => x) shouldBe Success(mList2)
  }

  "After a true comparison," should "apply the then function" in {
    val mList1 = Sortable(1, 0, 2)
    val mList2 = Sortable(List(0, 1, 2), List(Step.Comparison(0, 1), Step.Swap(0, 1)))
    mList1.compare(0, 1)(x => x.swap(0, 1).get)(x => x) shouldBe Success(mList2)
  }

  "After a false comparison," should "apply the else function" in {
    val mList1 = Sortable(0, 1, 2)
    val mList2 = Sortable(List(1, 0, 2), List(Step.Comparison(0, 1), Step.Swap(0, 1)))
    mList1.compare(0, 1)(x => x)(x => x.swap(0, 1).get) shouldBe Success(mList2)
  }

  "Swapping each element on (3,4,5) " should "give (4,5,3)" in {
    val mList1 = Sortable(3, 4, 5)
    val m2 = mList1.foreach(1 to 2)((l,i) => l.swap(i - 1, i).get)
    assert(m2.data == Seq(4, 5, 3))
  }

  "Swapping each element with double loop on (3,4,5) " should "give (5,4,3)" in {
    val mList1 = Sortable(3, 4, 5)
    val m2 = mList1.foreach(0 to 2)((l, i) =>
      l.foreach(i + 1 until l.length())((l2,j) =>
        l2.swap(i, j).get))
    assert(m2.data == Seq(5, 4, 3))
  }


