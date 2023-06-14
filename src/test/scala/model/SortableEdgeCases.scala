package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}
import model.Sortable

class SortableEdgeCases extends AnyFlatSpec with Matchers:

  import model.Sortable.*

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  "Swap on an empty list" should "fail" in {
    val list = Sortable()
    list.swap(3, 4) match
      case Failure(_) =>
      case Success(_) => fail()
  }

  "Swap(0,0) on an empty list" should "fail" in {
    val list = Sortable()
    list.swap(0, 0) match
      case Failure(e) =>
      case Success(_) => fail()
  }

  "A correct swap" should "not fail" in {
    val list = Sortable(0,1,5)
    list.swap(0, 2) match
      case Failure(_) => fail()
      case Success(l) => assert(l == Sortable(List(5,1,0), List(Step.Swap(0,2))))
  }

  "Select on an empty list" should "fail" in {
    val list = Sortable()
    list.select("test", 3) match
      case Failure(e) =>
      case Success(_) => fail()
  }

  "Select(0) on an empty list" should "fail" in {
    val list = Sortable()
    list.select("test", 0) match
      case Failure(e) =>
      case Success(_) => fail()
  }

  "A correct selection" should "not fail" in {
    val list = Sortable(0, 1, 5)
    list.select("test", 2) match
      case Failure(_) => fail()
      case Success(l) => assert(l == Sortable(List(0, 1, 5), List(Step.Selection("test", 2)), Map("test" -> 2)))
  }

  "A correct double selection" should "not fail" in {
    val list = Sortable(0, 1, 5)
    list.select("test", 2).get.select("test", 0) match
      case Failure(_) => fail()
      case Success(l) => assert(l == Sortable(List(0, 1, 5), List(Step.Selection("test", 2), Step.Selection("test", 0)), Map("test" -> 0)))
  }

  "A deselection" should "not fail" in {
    val list = Sortable(0, 1, 5)
    list.deselect("test") match
      case Failure(_) => fail()
      case Success(l) => assert(l == Sortable(List(0, 1, 5), List(Step.Deselection("test"))))
  }

  "A deselection after a selection" should "not fail" in {
    val list = Sortable(0, 1, 5)
    list.select("test", 2).get.deselect("test") match
      case Failure(_) => fail()
      case Success(l) => assert(l == Sortable(List(0, 1, 5), List(Step.Selection("test", 2), Step.Deselection("test"))))
  }

  "A correct comparison" should "not fail" in {
    val list = Sortable(0, 1, 5)
    list.compare(0, 1)(x => x)(x => x) match
      case Failure(_) => fail()
      case Success(l) => assert(l == Sortable(List(0, 1, 5), List(Step.Comparison(0, 1))))
  }

  "Compare(0, 1) on an single element list" should "fail" in {
    val list = Sortable(0)
    list.compare(0, 1)(x => x)(x => x) match
      case Failure(e) =>
      case Success(_) => fail()
  }


