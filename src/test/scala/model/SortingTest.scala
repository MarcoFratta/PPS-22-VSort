package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class SortingTest extends AnyFlatSpec with Matchers:

  import model.Sortable.*

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  "Bubble sort" should "work" in {
    val data = List(5, 4, 2, 1, 3)
    var mList1 = Sortable(data, List.empty)
    for (i <- 0 to mList1.length - 2) {
      for (j <- 0 to mList1.length - 2 - i) {
        mList1 = mList1.compare(j, j + 1)(x => x.swap(j, j + 1).get)(x => x).get
      }
    }

    StepsVisualizer.visualize(mList1.steps, data)

    mList1.data shouldBe Seq(1, 2, 3, 4, 5)
  }

//  "Selection sort" should "work" in {
//    var mList = Sortable(5, 4, 2, 1, 3)
//    for (i <- 0 to mList.length() - 2) {
//      mList = mList.select("min", i).get
//      for (j <- i + 1 until mList.length()) {
//        mList = mList.compare(mList.getSelection("min"), j)(x => x.select("min", j).get)(x => x).get
//      }
//      mList = mList.swap(mList.getSelection("min"), i).get
//    }
//    mList.data shouldBe Seq(1, 2, 3, 4, 5)
//  }

//  "Insertion sort" should "work" in {
//    var mList = Sortable(5, 4, 2, 1, 3)
//    for (i <- 1 until mList.length()) {
//      mList = mList.select("sel", i).get
//      for (j <- i - 1 to 0 by -1) {
//        mList = mList.compare(j, mList.getSelection("sel"))(x => x
//          .swap(mList.getSelection("sel"), j).get.select("sel", j).get)(x => x).get
//      }
//    }
//    mList.data shouldBe Seq(1, 2, 3, 4, 5)
//  }

  import model.SortingAlgorithms.*

  "Merge Sort" should "work" in {
    val list = List(5, 4, 2, 1, 3)
    val res = mergeSort(list)
    res shouldBe List(1, 2, 3, 4, 5)
  }
