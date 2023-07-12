package model

import model.sortModel.SortOperations.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.language.postfixOps


class SortableTest extends AnyFlatSpec with Matchers:

  import model.sortModel.SortOperations.given


  "A Sortable[Int,String] with steps" should "exists" in {
    Sortable[Int, String](Seq.empty)
  }

  "A swap " should "swap data" in {
    val s = Sortable[Int, String](Seq(5, 6, 7))
    for p1 <- s.swap(1, 0)
      yield p1.data shouldEqual Seq(6, 5, 7)
  }

  "A comparison " should "add one comparison step" in {

    val s = Sortable[Int, String](Seq(5, 6, 7))
    for p1 <- s.compare(0, 1)(x => x !)(x => x !)
      yield p1.steps shouldEqual Seq(Comparison(0, 1))
  }


  "If true, a comparison " should "execute true branch" in {
    val s = Sortable[Int, String](Seq(5, 6, 7))
    val k = for result <- s.compare(0, 1)(_.swap(0, 1))(_.swap(0, 2)) yield (result.data, result.steps)
    k.get._2 shouldEqual Seq(Comparison(0, 1), Step.Swap(0, 2))
    k.get._1 shouldEqual Seq(7, 6, 5)

  }

  //
  "A divide on correct indexes " should "add one divide step" in {
    val s = Sortable[Int, String](Seq(5, 6, 7))
    for p1 <- s.divide(1, 0)
      do
        p1.data shouldEqual Seq(5, 6, 7)
        p1.steps shouldEqual Seq(Step.Divide(1, 0))
  }

  "A divide on incorrect indexes " should "fail" in {
    val s = Sortable[Int, String](Seq(5, 6, 7))
    for p1 <- s.divide(1, 0)
      do
        p1.data shouldEqual Seq(5, 6, 7)
        p1.steps shouldEqual Seq(Step.Divide(1, 0))
  }

  "Divide(0, 1) on an single element list" should "fail" in {
    the[IllegalArgumentException] thrownBy {
      val list = Sortable[Int, String](Seq(0))
      for l <- list.divide(0, 1) do
        fail()
    } should have message "Invalid divide indexes (0 - 1)"
  }

  "An iteration on (1,2,3)" should "not modify data" in {
    val s = Sortable[Int, String](Seq(1, 2, 3))
    val y = (for i <- s.loopFor(0 to 3 by 1)
      yield i.prev).get.data
    y shouldEqual Seq(1, 2, 3)

  }

  "An iteration on (1,2,3)" should "should swap 3 times" in {
    val s = Sortable[Int, String](Seq(1, 2, 3))
    val y = (for i <- s.loopFor(0 to 1 by 1)
                 p2 <- i.prev.swap(i.index, i.index + 1)
    yield p2).get.data
    y shouldEqual Seq(2, 3, 1)

  }


  "A while (i < 3) on (1,2,3) incrementing i at each iteration" should "swap 3 times" in {
    val array = Sortable[Int, String](Seq(1, 2, 3, 4))
    var t = -1
    val y = (for l <- array.whileLoop(_ =>
      t += 1
      println(f"check $t")
      t < 3)
                 l2 <- l.swap(t, t + 1) yield l2).get

    y.data shouldEqual Seq(2, 3, 4, 1)
    t shouldEqual 3
  }

  "A while true on (1,2,3" should "stop after a swap and a break" in {
    val array = Loopable(Seq(1, 2, 3, 4))
    val y = (for l <- array.whileBreak(_ => true)
                 l2 <- l.swap(0, 3)
                 l3 <- l2.break
    yield l3).get

    y.data shouldEqual Seq(4, 2, 3, 1)
  }


  "Bubble sort with loop for" should "work" in {
    import StepsVisualizer.*


    val s = Sortable[Int, String](Seq(9, 8, 7, 6, 5, 4, 3, 2, 1, 0))
    val y = for i <- s.loopFor(0 to s.data.length - 2)
                j <- i.prev.loopFor(0 to i.prev.data.length - 2 - i.index)
                res <- j.prev.compare(j.index, j.index + 1)(x => x.swap(j.index, j.index + 1))(x => x)
    yield res

    y.get.data shouldEqual Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }







