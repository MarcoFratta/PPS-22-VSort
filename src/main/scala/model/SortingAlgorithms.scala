package model

import model.Step.Comparison
import model.sortModel.{SelectableM, Selections}
import model.sortModel.SortOperation.*
import model.sortModel.SortableFunctionalities.*
import model.sortModel.SortableM

import scala.language.postfixOps

object SortingAlgorithms:

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

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
                       (j, t2) => t2.compare(t2.getSelection(" min").get, j)(x =>
                         for p5 <- x.deselect(" min")
                             p6 <- p5.select(" min", j)
                         yield p6)
                       (x => x)
                     )
                     p3 <- p2.swap(p2.getSelection(" min").get, i)
                     p4 <- p3.deselect(" min")
      yield p4
    ) yield (res.data, res.steps)).get._2

  //  def insertionSort(seq: Seq[Int]): Seq[Step] = ???

  def mergeSort(seq: Seq[Int]): Seq[Step] = mergesort(SelectableM(seq), 0, seq.length-1)._1.steps


  private def mergesort[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) = end - start match
    case n if n < 1 => (seq, start, end)
    case n =>
      val t1 = mergesort(seq, start, start + (n / 2))
      val t2 = mergesort(t1._1, start + 1 + (n / 2), end)
      merge(t2._1, start, start + (n / 2), end)

  private def merge[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, mid: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) =

    given Comparable[Int] with
      override def compare(a: Int, b: Int): Boolean = b - a > 0

    val startl = start
    val startr = mid + 1

    println("metto in ordine da " + startl + " a " + end)

    var i = startl
    var j = startr

    var res = (for p1 <- seq.select("i", start)
                   p2 <- p1.select("j", mid + 1)
    yield p2).get

    while (j < end) {

      res = (for p1 <- res.compare(res.getSelection("i").get, res.getSelection("j").get)(x =>
        i = i + 1
        for
          p2 <- x.select("k", x.getSelection("i").get)
          p3 <- p2.deselect("i")
          p4 <- p3.select("i", p3.getSelection("k").get + 1)
          p5 <- p4.deselect("k")
        yield p5)(x =>
        i = i + 1
        j = j + 1
        for
          p2 <- x.iterate(x.getSelection("j").get until x.getSelection("i").get by -1)(
            (k, t) => for p3 <- t.swap(k, k - 1)
                          p4 <- p3.select("k", p3.getSelection("i").get)
                          p5 <- p4.deselect("i")
                          p6 <- p5.select("i", p5.getSelection("k").get + 1)
                          p7 <- p6.deselect("k")
                          p8 <- p7.select("k", p7.getSelection("j").get)
                          p9 <- p8.deselect("j")
                          p10 <- p9.select("j", p9.getSelection("k").get + 1)
                          p11 <- p10.deselect("k")
            yield p11
          ) yield p2
      ) yield p1).get
    }

    println("messo in ordine da " + startl + " a " + end)

    (res, startl, end)