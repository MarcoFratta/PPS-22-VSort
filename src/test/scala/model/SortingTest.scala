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

    print("Bubble sort\n" + string)
    result shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Selection sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = selectionSort(data)
    val (result, string) = visualizeSteps(steps, data)

    print("Selection sort\n" + string)
    result shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Insertion sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = insertionSort(data)
    val (result, string) = visualizeSteps(steps, data)

    print("Insertion Sort\n" + string)
    result shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Merge Sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = mergeSort(data)
    val (result, string) = visualizeSteps(steps, data)

    print("Merge sort\n" + string)
    result shouldBe Seq(10, 20, 30, 40, 50, 60)
  }
