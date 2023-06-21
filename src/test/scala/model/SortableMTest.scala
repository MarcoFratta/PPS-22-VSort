package model

import model.SortableFunctionalities.*
import model.Step.Comparison
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps


class SortableMTest extends AnyFlatSpec with Matchers:

  import SortOperation.*

  given comp: Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  "A sortable with steps" should "exists" in {
    SortableM()
  }

  "A comparison " should "add one comparison step" in {
    given Conversion[Steps[Int], SortOps[Steps[Int]]] = _ !

    val s = SortableM(5, 6, 7)
    for p1 <- s.compare(0, 1)(x => x)(x => x !)
      yield p1.steps shouldEqual Seq(Comparison(0, 1))
  }

  "A swap " should "swap data" in {
    val s = SortableM(5, 6, 7)
    for p1 <- s.swap(1, 0)
      yield p1.data shouldEqual Seq(6, 5, 7)
  }

  "If true, a comparison " should "execute true branch" in {
    val s = SortableM(5, 6, 7)
    val k = for result <- s.compare(0, 1)(ifTrue =>
      s.swap(0, 1))(ifFalse =>
      s.swap(0, 2)) yield (result.data, result.steps)


    k.get._2 shouldEqual Seq(Comparison(0, 1), Step.Swap(0, 2))
    k.get._1 shouldEqual Seq(7, 6, 5)

  }


  "An iteration on (1,2,3)" should "iterate 3 times over data" in {
    val s = SortableM(1,2,3)
    val y = for p1 <- s.iterate(0 to 3 by 1)((i,t) => t.swap(0,1))
      yield p1
  }

  "An iteration on (1,2,3)" should "swap 3 times" in {
    val array = SortableM(1, 2, 3)
    val y = for l <- array.iterate(0 to 1 by 1)((i, t) =>
      println(f"i -> $i t-> ${t.data}")
      t.swap(i, i + 1)) // meglio i <- o (i,array) <- ???
    yield ()
    array.data shouldEqual Seq(2, 3, 1)
  }

  "Bubble sort" should "work" in {
    import StepsVisualizer.*
    val s = SortableM(3, 2, 1, 5, 8, 3, 5, 0, 1)
    val y = for i <- s.iterate(0 to s.length - 2)(
      (i, t) => t.iterate(0 to t.length - 2 - i)(
        (j, t2) => t2.compare(j, j + 1)(x => x.swap(j, j + 1))(_ !)))
    yield (i.data, i.steps)

    println(visualizeSteps(y.get._2, y.get._1))
    y.get._1 shouldBe Seq(0, 1, 1, 2, 3, 3, 5, 5, 8)
  }
