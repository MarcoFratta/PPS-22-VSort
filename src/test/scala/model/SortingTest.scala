package model

import model.SortingAlgorithms.*
import model.StepsVisualizer
import model.sortModel.SortOperations.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps
import scala.util.{Failure, Success}

class SortingTest extends AnyFlatSpec with Matchers:

  import model.sortModel.SortOperations.given

  val visualizer = StepsVisualizer[Int]

  "Bubble sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = bubbleSort(data)

    print("Bubble sort\n" + visualizer.getString(steps, data))
    visualizer.getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Selection sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = selectionSort(data)

    print("Selection sort\n" + visualizer.getString(steps, data))
    visualizer.getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Insertion sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = insertionSort(data)

    print("Insertion Sort\n" + visualizer.getString(steps, data))
    visualizer.getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Merge Sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = mergeSort(data)

    print("Merge sort\n" + visualizer.getString(steps, data))
    visualizer.getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Heap Sort" should "work" in {
    val data = Seq(60, 20, 30, 40, 50, 10)
    val steps = heapSort(data)

    print("Heap sort\n" + visualizer.getString(steps, data))
    visualizer.getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

  "Quick Sort" should "work" in {
    val data = Seq(20, 60, 10, 40, 50, 30)
    val steps = quickSort(data)

    print("Quick sort\n" + visualizer.getString(steps, data))
    visualizer.getResult(steps, data) shouldBe Seq(10, 20, 30, 40, 50, 60)
  }

