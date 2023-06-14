package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}
import model.Sortable

class SortingTest extends AnyFlatSpec with Matchers:

  import model.Sortable.*

  "Bubble sort" should "work" in {
    var mList1 = Sortable(5, 4, 2, 1, 3)
    for (_ <- 0 to mList1.length() - 2) {
      for (j <- 0 to mList1.length() - 2) {
        mList1 = mList1.compare(j, j + 1)(x => x.swap(j, j + 1).get)(x => x)(using _ - _ > 0).get
      }
    }
    mList1.data shouldBe Seq(1, 2, 3, 4, 5)
  }