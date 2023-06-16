package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Failure, Success, Try}

class SelectableBehaviourTest extends AnyFlatSpec with Matchers:
  import model.Selectable.*

  behavior of "A selectable"

  it should "exists" in {
    val mList = Selectable(Seq(0, 1, 2), Seq.empty)
  }

  "After a swap, it" should "have a swap step and the elements must be swapped" in {
    val mList1 = Selectable(Seq(0, 1, 2), Seq.empty)
    val mList2 = Selectable(List(1, 0, 2), List(Step.Swap(0,1)))
    mList1.swap(0,1) shouldBe Success(mList2)
  }

  "After a selection, it" should "have 1 selection step and the same data" in {
    val mList1 = Selectable(Seq(0, 1, 2), Seq.empty)
    val mList2 = Selectable(List(0, 1, 2), List(Step.Selection("test", 0)), Map("test" -> 0))
    mList1.select("test", 0) shouldBe Success(mList2)
  }

  "After a deselection," should "have 1 deselection step and the same data" in {
    val mList1 = Selectable(Seq(0, 1, 2), Seq.empty)
    val mList2 = Selectable(List(0, 1, 2), List(Step.Deselection("test")))
    mList1.deselect("test") shouldBe Success(mList2)
  }

  "An empty list" should "have size 0" in {
    Selectable().length() shouldBe 0
  }

  "A list with 2 elements" should "have size 2" in {
    Selectable(Seq(3,4), Seq.empty).length() shouldBe 2
  }

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  "After a comparison," should "have the same data and have a compare step" in {
    val mList1 = Selectable(Seq(1, 0, 2), Seq.empty)
    val mList2 = Selectable(List(1, 0, 2), List(Step.Comparison(0, 1)))
    mList1.compare(0, 1)(x => x)(x => x) shouldBe Success(mList2)
  }

  "After a true comparison," should "apply the then function" in {
    val mList1 = Selectable(Seq(1, 0, 2), Seq.empty)
    val mList2 = Selectable(List(0, 1, 2), List(Step.Comparison(0, 1), Step.Swap(0, 1)))
    mList1.compare(0, 1)(x => x.swap(0, 1).get)(x => x) shouldBe Success(mList2)
  }

  "After a false comparison," should "apply the else function" in {
    val mList1 = Selectable(Seq(0, 1, 2), Seq.empty)
    val mList2 = Selectable(List(1, 0, 2), List(Step.Comparison(0, 1), Step.Swap(0, 1)))
    mList1.compare(0, 1)(x => x)(x => x.swap(0, 1).get) shouldBe Success(mList2)
  }

