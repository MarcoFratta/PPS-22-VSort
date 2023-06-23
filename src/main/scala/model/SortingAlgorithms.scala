package model

import model.Step.Comparison
import model.sortModel.SortOperation.*
import model.sortModel.SortableFunctionalities.*
import model.sortModel.*
import model.sortModel.SortAddOns.IterateOps
import model.sortModel.{SelectableM, Selections, SortableM}

import scala.language.postfixOps

object SortingAlgorithms:
  import model.sortModel.SortAddOns.IterateOps

  def bubbleSort(seq: Seq[Int]): Seq[Step] =
    given Comparable[Int] with
      override def compare(a: Int, b: Int): Boolean = a - b > 0
    given Conversion[Steps[Int], SortOps[Steps[Int]]] = _ !

    (for i <- SortableM(seq).loopFor(0 to seq.length - 2)
         j <- i.previous.loopFor(0 to seq.length - 2 - i.value)
         p1 <- j.previous.compare(j.value, j.value + 1)(x => x.swap(j.value, j.value + 1))(x => x)
      yield p1).get.steps

  def selectionSort(seq: Seq[Int]): Seq[Step] =
    given Comparable[Int] with
      override def compare(a: Int, b: Int): Boolean = a - b > 0
    given Conversion[Steps[Int] with Selections[String, Int], SortOps[Steps[Int] with Selections[String, Int]]] = _ !

    (for i <- SelectableM(seq).loopFor(0 to seq.length - 2)
         p1 <- i.previous.select("min", i.value)
         p2 <- for j <- p1.loopFor(i.value + 1 until seq.length)
                  p3 <- j.previous.compare(j.previous -> "min", j.value)(x =>
                    x.select("min", j.value))(x => x)
              yield p3
         min <- p2.getSelection("min")
         p4 <- p2.deselect("min")
         p5 <- p4.swap(min.get, i.value)
      yield p5).get.steps

  def insertionSort(seq: Seq[Int]): Seq[Step] =

    given Comparable[Int] with
      override def compare(a: Int, b: Int): Boolean = a - b > 0
    given Conversion[Steps[Int] with Selections[String, Int], SortOps[Steps[Int] with Selections[String, Int]]] = _ !

    (for i <- SelectableM(seq).loopFor(1 until seq.length)
         p1 <- i.previous.select("sel", i.value)
         p2 <- for j <- p1.loopFor(i.value - 1 to 0 by -1)
                  p3 <- j.previous.compare(j.value, j.previous -> "sel")(x =>
                    for p4 <- x.swap(x -> "sel", j.value)
                        p5 <- p4.select("sel", j.value)
                    yield p5)(x => x)
                yield p3
         p3 <- p2.deselect("sel")
    yield p3).get.steps

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = b - a > 0

  def mergeSort(seq: Seq[Int]): Seq[Step] = mergesort(SelectableM(seq), 0, seq.length-1)._1.steps

  private def mergesort[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) = end - start match
    case n if n < 1 => ((for p1 <- seq.divide(start, end) yield p1).get, start, end)
    case n =>
      ((for p1 <- seq.divide(start, end)
            p2 <- mergesort(p1, start, start + (n / 2))._1.!
            p3 <- p2.divide(start + 1 + (n / 2), end)
            p4 <- mergesort(p3, start + 1 + (n / 2), end)._1.!
            p5 <- merge(p4, start, start + (n / 2), end)._1.!
      yield p5).get, start, end)

  private def merge[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, mid: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) =
    ((for p1 <- seq.select("i", start)
          p2 <- p1.select("j", mid + 1)
          r <- p2.loopWhile(h => h -> "j" < end + 1 && h -> "i" != h -> "j")(g =>
            for p1 <- g.compare(g -> "i", g -> "j")(x =>
              for
                i <- x.getSelection("i")
                p2 <- x.select("i", i.get + 1)
              yield p2)(x => for p2 <- x.iterate(x -> "j" until x -> "i" by -1)(
              (k, t) => for p3 <- t.swap(k, k - 1)
                yield p3)
                                 i <- p2.getSelection("i")
                                 p3 <- p2.select("i", i.get + 1)
                                 j <- p3.getSelection("j")
                                 p4 <- p3.select("j", j.get + 1)
            yield p4
            ) yield p1)
          p3 <- r.deselect("i")
          p4 <- p3.deselect("j")
    yield p4).get, start, end)
