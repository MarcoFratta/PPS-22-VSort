package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class MyTest extends AnyFlatSpec with Matchers:

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

//  "Bubble sort" should "work" in {
//    val mList1 = IterableS(3,2,1)
//    val y =  mList1.foreach(0 to mList1.length - 2)((l,i) =>
//                      l.foreach(0 to l.length - 2)((l2,j) =>
//                        l2.compare(i, j + 1)(x => x.swap(i, j + 1).get)(x => x).get))
//    y.steps.foreach(println(_))
//    y.data shouldBe Seq(1, 2, 3)
//    }

