package model

import model.SortableFunctionalities.*
import model.Step.Comparison
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SortableMTest extends AnyFlatSpec with Matchers:

  import SortOperation.*

  given comp:Comparable[Int] with
      override def compare(a: Int, b: Int): Boolean = a - b > 0

  "A sortable with steps" should "exists" in {
    SortableM()
  }

  "A comparison " should "add one comparison step" in {
    val s = SortableM(5, 6, 7)
    for p1 <- s.compare(0, 1)(x => x)(x => x)
          yield p1.steps shouldEqual Seq(Comparison(0,1))
 }

  "A swap " should "swap data" in {
    val s = SortableM(5, 6, 7)
    for p1 <- s.swap(1, 0)
      yield p1.data shouldEqual Seq(6, 5, 7)
  }

  "If true, a comparison " should "execute true branch" in {
    val s = SortableM(5, 6, 7)
    for result <- s.compare(0, 1)(ifTrue =>
      for t <-  s.swap(0,1) yield s)(ifFalse =>
      for t <-  s.swap(0,2) yield s) yield ()

    s.steps shouldEqual Seq(Comparison(0,1), Step.Swap(0,2))
    s.data shouldEqual Seq(7,6,5)

  }



  "An iteration on (1,2,3)" should "iterate 3 times over data" in {
    val s = SortableM(1,2,3)
    val y = for p1 <- s.iterate(0 to 3 by 1)((i,t) => t.swap(0,1))
      yield p1
  }

  "An iteration on (1,2,3)" should "swap 3 times" in {
    val array = SortableM(1, 2, 3)
    val y = for l <- array.iterate(0 to 1 by 1)((i,t) =>
      println(f"i -> $i t-> ${t.data}")
      t.swap(i, i + 1)) // meglio i <- o (i,array) <- ???
                yield ()
    array.data shouldEqual Seq(2,3,1)
  }

//  "Bubble sort" should "work" in {
//      val mList1 = SortableM(3,2,1)
//      val y = for i <- mList1.iterate(0 to mList1.length - 2)((i, t) => {
//        for j <- t.iterate(0 to t.length - 2 - i)((j, t2) => {
//          t2.compare(j, j + 1)(x =>
//                    print("i -> " + i)
//                    x.swap(j, j + 1))(x =>
//                    print("j -> " + j)
//                    x).get
//        }) yield ()
//      }) yield ()
//
//
//      mList1.steps.foreach(println)
//      mList1.data shouldBe Seq(1,2,3)
    //}

