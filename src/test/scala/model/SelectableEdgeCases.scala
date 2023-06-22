package model

import model.sortModel.SelectableM
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps
import scala.util.{Failure, Success}

class SelectableEdgeCases extends AnyFlatSpec with Matchers:

  import model.Step.*
  import model.sortModel.SortOperation.*
  import model.sortModel.SortableFunctionalities.*

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  "Swap on an empty list" should "not do anything" in {
    val list = SelectableM(Seq.empty)
    list.swap(3, 4).get.steps shouldEqual Seq.empty
  }

  "Swap(0,0) on an empty list" should "not do anything" in {
    val list = SelectableM(Seq.empty)
    list.swap(0, 0).get.steps shouldEqual Seq.empty
  }

  "Select on an empty list" should "not do anything" in {
    val list = SelectableM(Seq.empty)
    list.select("test", 3).get.steps shouldEqual Seq.empty
  }

  "Select(0) on an empty list" should "not do anything" in {
    val list = SelectableM(Seq.empty)
    list.select("test", 0).get.steps shouldEqual Seq.empty
  }

  "A correct selection" should "not fail" in {
    val list = SelectableM(Seq(0, 1, 5))
    for l <- list.select("test", 2) do
      l.data shouldEqual List(0, 1, 5)
      l.steps shouldEqual List(Selection("test", 2))
      l.get("test") shouldBe Option(2)

  }

  "A correct double selection" should "not fail" in {
    val list = SelectableM(Seq(0, 1, 5))
    for l <- list.select("test", 2)
        l2 <- l.select("test2", 0) do
      l2.data shouldEqual List(0, 1, 5)
      l2.steps shouldEqual List(Selection("test", 2), Selection("test2", 0))
      l2.get("test") shouldBe Option(2)
  }

  "A deselection" should "not fail" in {
    val list = SelectableM(Seq(0, 1, 5))
    for l <- list.deselect("test") do
      l.data shouldEqual List(0, 1, 5)
      l.steps shouldEqual List(Deselection("test"))
  }

  "A deselection after a selection" should "not fail" in {
    val list = SelectableM(Seq(0, 1, 5))
    for l <- list.select("test", 2)
        l2 <- l.deselect("test") do
      l2.data shouldEqual List(0, 1, 5)
      l2.steps shouldEqual List(Selection("test", 2), Deselection("test"))

  }

  "A correct comparison" should "not fail" in {
    val list = SelectableM(Seq(0, 1, 5))
    for l <- list.compare(0, 1)(x => x !)(x => x !) do
      l.data shouldEqual List(0, 1, 5)
      l.steps shouldEqual List(Comparison(0, 1))

  }

  "Compare(0, 1) on an single element list" should "fail" in {
    the[IllegalArgumentException] thrownBy {
      val list = SelectableM(Seq(0))
      for l <- list.compare(0, 1)(x => x.select("x", 0))(x => x.select("x", 1)) do
        l.steps shouldEqual Seq.empty
        l.data shouldEqual Seq(0)
    } should have message "Invalid index"

  }


