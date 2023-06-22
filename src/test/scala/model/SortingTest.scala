package model

import model.SortingAlgorithms.*
import model.StepsVisualizer.*
import model.sortModel.{SortableM, SelectableM, Selections}
import model.sortModel.SortableFunctionalities.*
import model.sortModel.SortOperation.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps
import scala.util.{Failure, Success}

class SortingTest extends AnyFlatSpec with Matchers:

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0
  given Conversion[Steps[Int] with Selections[String, Int], SortOps[Steps[Int] with Selections[String, Int]]] = _ !

  "Bubble sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = bubbleSort(data)
    val (result, string) = visualizeSteps(steps, data)

    print(string)
    result shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Selection sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = selectionSort(data)
    val (result, string) = visualizeSteps(steps, data)

    print(string)
    result shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

//  "Insertion sort" should "work" in {
//    var mList = Selectable(5, 4, 2, 1, 3)
//    for (i <- 1 until mList.length) {
//      mList = mList.select("sel", i).get
//      for (j <- i - 1 to 0 by -1) {
//        mList = mList.compare(j, mList.getSelection("sel"))(x => x
//          .swap(mList.getSelection("sel"), j).get.deselect("sel").get.select("sel", j).get)(x => x).get
//      }
//      mList = mList.deselect("sel").get
//    }
//
//    print(visualizeSteps(mList.steps, Seq(5, 4, 2, 1, 3))._2)
//
//    mList.data shouldBe Seq(1, 2, 3, 4, 5)
//  }

  "Merge Sort" should "work" in {
    val data = Seq(20, 30, 40, 10, 80, 140, 12)
    val steps = mergeSort(data)
    //val (result, string) = visualizeSteps(steps, data)

    //print(string)
    steps shouldBe Seq(10, 12, 20, 30, 40, 80, 140)
  }
