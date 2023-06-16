package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Failure, Success, Try}

class SelectableBehaviourTest extends AnyFlatSpec with Matchers:
  import model.Selectable.*
  import model.Step.*

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0


  behavior of "A selectable"

  it should "exists" in {
    val mList = Selectable(0,1,2)
  }

  "After a swap, it" should "have a swap step and the elements must be swapped" in {
    val mList1 = Selectable(0,1,2).swap(0,1).get
    mList1.data shouldBe Seq(1, 0, 2)
    mList1.steps shouldBe Seq(Swap(0,1))
  }

  "After a selection, it" should "have 1 selection step and the same data" in {
    val mList1 = Selectable(0, 1, 2).select("test", 0).get
    mList1.data shouldEqual Seq(0, 1, 2)
    mList1.steps shouldEqual Seq(Selection("test", 0))
    mList1.getSelection("test") shouldBe 0
  }

  "After a deselection," should "have 1 deselection step and the same data" in {
    val mList1 = Selectable(0, 1, 2).deselect("test") match
      case Failure(exception) => fail()
      case Success(value) => value
        //val mList2 = Selectable(List(0, 1, 2), List(Deselection("test")))
      mList1.data shouldEqual Seq(0, 1, 2)
      mList1.steps shouldEqual Seq(Deselection("test"))
  }

  "An empty list" should "have size 0" in {
    Selectable().length shouldBe 0
  }

  "A list with 2 elements" should "have size 2" in {
    Selectable(3,4).length shouldBe 2
  }


  "After a comparison," should "have the same data and have a compare step" in {
    val mList1 = Selectable(1, 0, 2).compare(0, 1)(x => x)(x => x).get
    mList1.data shouldEqual Seq(1,0,2)
    mList1.steps shouldEqual Seq(Comparison(0,1))
  }

  "After a true comparison," should "apply the then function" in {
    val mList1 = Selectable(1, 0, 2).compare(0, 1)(x => x.swap(0, 1).get)(x => x).get
    mList1.data shouldEqual Seq(0,1,2)
    mList1.steps shouldEqual Seq(Comparison(0, 1), Swap(0, 1))
  }

  "After a false comparison," should "apply the else function" in {
    val mList1 = Selectable(0, 1, 2).compare(0, 1)(x => x)(x => x.swap(0, 1).get).get
    mList1.data shouldBe Seq(1,0,2)
    mList1.steps shouldBe Seq(Comparison(0, 1), Swap(0, 1))
  }

