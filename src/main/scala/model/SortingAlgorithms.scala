package model

import model.Step.Comparison
import model.sortModel.{SelectableM, Selections}
import model.sortModel.SortOperation.*
import model.sortModel.SortableFunctionalities.*
import model.sortModel.SortableM

import scala.language.postfixOps

object SortingAlgorithms:

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = b - a > 0

  def bubbleSort(seq: Seq[Int]): Seq[Step] =
    given Conversion[Steps[Int], SortOps[Steps[Int]]] = _ !

    (for res <- SortableM(seq).iterate(0 to seq.length - 2)(
      (i, t) => t.iterate(0 to t.length - 2 - i)(
        (j, t2) => t2.compare(j, j + 1)(x => x.swap(j, j + 1))(x => x)))
    yield (res.data, res.steps)).get._2

  def selectionSort(seq: Seq[Int]): Seq[Step] =
    given Conversion[Steps[Int] with Selections[String, Int], SortOps[Steps[Int] with Selections[String, Int]]] = _ !

    (for res <- SelectableM(seq).iterate(0 to seq.length - 2)(
      (i, t1) => for p1 <- t1.select(" min", i)
                     p2 <- p1.iterate(i + 1 until seq.length)(
                       (j, t2) => t2.compare(t2.get(" min").get, j)(x =>
                         for p5 <- x.deselect(" min")
                             p6 <- p5.select(" min", j)
                         yield p6)
                       (x => x)
                     )
                     p3 <- p2.swap(p2.get(" min").get, i)
                     p4 <- p3.deselect(" min")
      yield p4
    ) yield (res.data, res.steps)).get._2

  //  def insertionSort(seq: Seq[Int]): Seq[Step] = ???

  def mergeSort(seq: Seq[Int]): Seq[Int] = mergesort(SelectableM(seq), 0, seq.length-1)._1.data

  private def mergesort[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) = end - start match
    case n if n < 1 => (seq, start, end)
    case n =>
      val t1 = mergesort(seq, start, start + (n / 2))
      val t2 = mergesort(t1._1, start + 1 + (n / 2), end)
      merge(t2._1, start, start + (n / 2), end)

  private def merge[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, mid: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) =

    var i = start
    var j = mid + 1

    var res = (for p1 <- seq.select("i", start)
                   p2 <- p1.select("j", mid + 1)
    yield p2).get

    while (j < end + 1 && i != j) {

      res = (for p1 <- res.compare(res.get("i").get, res.get("j").get)(x =>
        i = i + 1
        for
          i <- x.getSelection("i")
          p2 <- x.deselect("i")
          p3 <- p2.select("i", i.get + 1)
        yield p3)(x =>
        i = i + 1
        j = j + 1
        for
          p2 <- x.iterate(x.get("j").get until x.get("i").get by -1)(
            (k, t) => for p3 <- t.swap(k, k - 1)
            yield p3)
          i <- p2.getSelection("i")
          p3 <- p2.deselect("i")
          p4 <- p3.select("i", i.get + 1)
          j <- p4.getSelection("j")
          p5 <- p4.deselect("j")
          p6 <- p5.select("j", j.get + 1)
        yield p6
      ) yield p1).get
    }

    ((for p1 <- res.deselect("i")
      p2 <- p1.deselect("j")
      yield p2).get, start, end)