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

    print("Bubble sort\n" + getString(steps, data))
    getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Selection sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = selectionSort(data)

    print("Selection sort\n" + getString(steps, data))
    getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Insertion sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = insertionSort(data)

    print("Insertion Sort\n" + getString(steps, data))
    getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Merge Sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = mergeSort(data)

    print("Merge sort\n" + getString(steps, data))
    println(getMapList(steps, data))
    getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Heap Sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = heapSort(data)

    print("Heap sort\n" + getString(steps, data))
    getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "getMapList" should "work" in {
    val data = Seq(20, 30, 10)
    val steps = bubbleSort(data)

    println("Bubble sort\n" + getString(steps, data))
    println(getMapList(steps, data))
  }