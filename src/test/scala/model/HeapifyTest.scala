package model

import model.SortingAlgorithms.heapify
import model.StepsVisualizer.getSteps
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class HeapifyTest extends AnyFlatSpec with Matchers:

  "A sequence (4, 10, 3, 5, 1) " should "be (10, 5, 3, 4, 1) after heapify" in {
    val data = Seq(4,10,3,5,1)
    val steps = heapify(data)
    val result = getSteps(steps, data)._1

    result shouldBe Seq(10, 5, 3, 4, 1)
  }

  "A sequence (7, 6, 5, 4, 3, 2, 1) " should "be (7, 6, 5, 4, 3, 2, 1) after heapify" in {
    val data = Seq(7, 6, 5, 4, 3, 2, 1)
    val steps = heapify(data)
    val result = getSteps(steps, data)._1

    result shouldBe Seq(7, 6, 5, 4, 3, 2, 1)
  }

  "A sequence (5, 4, 3, 2, 1) " should "be (5, 4, 3, 2, 1) after heapify" in {
    val data = Seq(5, 4, 3, 2, 1)
    val steps = heapify(data)
    val result = getSteps(steps, data)._1

    result shouldBe Seq(5, 4, 3, 2, 1)
  }

  "A sequence (2, 8, 5, 3, 9, 1, 6) " should "be (9, 8, 6, 3, 2, 1, 5) after heapify" in {
    val data = Seq(2, 8, 5, 3, 9, 1, 6)
    val steps = heapify(data)
    val result = getSteps(steps, data)._1

    result shouldBe Seq(9, 8, 6, 3, 2, 1, 5)
  }