package model

import model.sortModel.{SelectableM, SortableFunctionalities, SortableM}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class SelectableBehaviourTest extends AnyFlatSpec with Matchers:

  import model.Step.*
  import model.sortModel.SortOperation.*
  import model.sortModel.SortableFunctionalities.*

  given Conversion[SortOps[(Seq[Int], Seq[Step])], (Seq[Int], Seq[Step])] = _.get

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0


  behavior of "A selectable"

  it should "exists" in {
    val mList = SelectableM(Seq(0, 1, 2))
  }

  "After a selection, it" should "have 1 selection step and the same data" in {
    val mList1 = SelectableM(Seq(0, 1, 2))

    val s = (for p1 <- mList1.select("test", 0) yield p1).get
    s.data shouldEqual Seq(0, 1, 2)
    s.steps shouldEqual Seq(Selection("test", 0))
    s.getSelection("test") shouldEqual Some(0)
  }

  "After a deselection," should "have 1 deselection step and the same data" in {
    val mList1 = SelectableM(Seq(0, 1, 2))
    val s = (for p1 <- mList1.deselect("test") yield p1).get
    mList1.data shouldEqual Seq(0, 1, 2)
    mList1.steps shouldEqual Seq(Deselection("test"))
  }

  "An empty list" should "have size 0" in {
    SelectableM(Seq.empty).length shouldBe 0
  }

  "A list with 2 elements" should "have size 2" in {
    SelectableM(Seq(3, 4)).length shouldBe 2
  }


  "After a comparison," should "have the same data and have a compare step" in {
    val mList1 = SelectableM(Seq(1, 0, 2)).compare(0, 1)(_ !)(_ !).get
    mList1.data shouldEqual Seq(1,0,2)
    mList1.steps shouldEqual Seq(Comparison(0,1))
  }

  "After a true comparison," should "apply the then function" in {
    val mList1 = SelectableM(Seq(1, 0, 2)).compare(0, 1)(_.swap(0, 1))(_ !).get
    mList1.data shouldEqual Seq(0, 1, 2)
    mList1.steps shouldEqual Seq(Comparison(0, 1), Swap(0, 1))
  }

  "After a false comparison," should "apply the else function" in {
    val mList1 = SelectableM(Seq(0, 1, 2)).compare(0, 1)(_ !)(_.swap(0, 1)).get
    mList1.data shouldBe Seq(1, 0, 2)
    mList1.steps shouldBe Seq(Comparison(0, 1), Swap(0, 1))
  }

  "After 2 selections, and 1 deselection it" should "have 2 selection,1  deselection " +
    "step and the same data" in {

    val s = (for p1 <- SelectableM(Seq(0, 1, 2)).select("test", 0)
                 p2 <- p1.select("test2", 1)
                 p3 <- p2.deselect("test2") yield p3).get
    s.data shouldEqual Seq(0, 1, 2)
    s.steps shouldEqual Seq(Selection("test", 0), Selection("test2", 1), Deselection("test2"))
    s.getSelection("test") shouldEqual Some(0)
  }

  "After 1 selection the getSelection" should "give the selected index" in {

    val s = (for p1 <- SelectableM(Seq(0, 1, 2)).select("test", 0)
                 p2 <- p1.select("test2", 1) yield p2.getSelection("test")).get
    s shouldEqual Some(0)
  }

  "After 1 selection and 1 deselection it" should "give no selections" in {
    for p1 <- SelectableM(Seq(0, 1, 2)).select("test", 0)
        p2 <- p1.deselect("test")
    do p2.getSelection("test") shouldEqual Option.empty
  }

