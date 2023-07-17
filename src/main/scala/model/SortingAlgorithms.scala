package model

import model.sortModel.Loopable.LoopableS
import model.sortModel.SortOperations.*

import scala.language.postfixOps


object SortingAlgorithms:

  import model.sortModel.SortOperations.{*, given}


  def bubbleSort[T: Comparable](seq: Seq[T]): Seq[Step] =

    (for i <- Loopable[T, String](seq).loopFor(0 to seq.length - 2)
         j <- i.prev.loopFor(0 to seq.length - 2 - i.index)
         p1 <- j.prev.compare(j.index, j.index + 1)(x => x.swap(j.index, j.index + 1))(x => x)
    yield p1).get.steps

  def selectionSort[T: Comparable](seq: Seq[T]): Seq[Step] =

    (for i <- Loopable[T, String](seq).loopFor(0 to seq.length - 2)
         p1 <- i.prev.select("min", i.index)
         p2 <- for j <- p1.loopFor(i.index + 1 until seq.length)
                   l1 <- j.prev.compare(j.prev -> "min", j.index)(x =>
                     x.select("min", j.index))(x => x)
         yield l1
         min <- p2.getSelection("min")
         p3 <- p2.deselect("min")
         p4 <- p3.swap(min.get, i.index)
    yield p4).get.steps

  def insertionSort[T: Comparable](seq: Seq[T]): Seq[Step] =

    (for i <- Loopable[T, String](seq).loopFor(1 until seq.length)
         p1 <- i.prev.select("sel", i.index)
         p2 <- for j <- p1.loopFor(i.index - 1 to 0 by -1)
                   l1 <- j.prev.compare(j.index, j.prev -> "sel")(x =>
                     for ll1 <- x.swap(x -> "sel", j.index)
                         ll2 <- ll1.select("sel", j.index)
                     yield ll2)(x => x) // x => x con break
         yield l1
         p3 <- p2.deselect("sel")
    yield p3).get.steps

  def heapSort[T: Comparable](seq: Seq[T]): Seq[Step] =
    (for p1 <- for i <- Loopable[T, String](seq).loopFor(seq.length / 2 - 1 to 0 by -1)
                   l1 <- heapify(i.prev, seq.length, i.index)
    yield l1
         p2 <- for i <- p1.loopFor(p1.data.length - 1 to 0 by -1)
                   l1 <- i.prev.swap(i.index, 0)
                   l2 <- heapify(l1, i.index, 0).!
         yield l2
    yield p2).get.steps

  def heapify[T: Comparable](seq: Seq[T]): Seq[Step] =
    (for i <- Loopable[T, String](seq).loopFor(seq.length / 2 - 1 to 0 by -1)
         l1 <- heapify(i.prev, seq.length, i.index)
    yield l1).get.steps

  def mergeSort[T: Comparable](seq: Seq[T]): Seq[Step] =
    mergeSort(Loopable[T, String](seq), 0, seq.length - 1)._1.steps

  def quickSort[T: Comparable](seq: Seq[T]): Seq[Step] =
    quickSort(Loopable[T, String](seq), 0, seq.length -1).steps

  private def quickSort[T: Comparable](seq: LoopableS[T, String], start: Int, end: Int): LoopableS[T, String] =
    if start < end then (for a1 <- seq.divide(start, end).get.deselect("i")
                             a2 <- partition(a1, start, end).!
                             a3 <- quickSort(a2, start, a2 -> "i" - 1).!
                             a4 <- quickSort(a3, a2 -> "i" + 1, end).!
                             a5 <- a4.deselect("i").get.divide(start, end)
                         yield a5).get else seq

  private def partition[T: Comparable](seq: LoopableS[T, String], start: Int, end: Int): LoopableS[T, String] =
    (for p1 <- seq.select("pi", end)
         p2 <- p1.select("i", start - 1)
         p3 <- for j <- p2.loopFor(start until end)
                  l1 <- j.prev.compare(j.prev -> "pi", j.index)(x => for a1 <- x.select("i", x -> "i" + 1)
                                                                         a2 <- a1.swap(a1 -> "i", j.index)
                                                                     yield a2)(x => x)
                yield l1
         p4 <- p3.select("i", p3 -> "i" + 1)
         p5 <- p4.swap(p4 -> "i", end)
    yield p5).get



  private def heapify[T: Comparable](seq: LoopableS[T, String], n: Int, i: Int):
  LoopableS[T, String] =

    (for p1 <- seq.select("max", i)
         p2 <- if 2 * i + 1 < n then p1.compare(2 * i + 1, p1 -> "max")(_.select("max", 2 * i + 1))(_ !) else p1.!
         p3 <- if 2 * i + 2 < n then p2.compare(2 * i + 2, p2 -> "max")(_.select("max", 2 * i + 2))(_ !) else p2.!
         p4 <- if i != (p3 -> "max") then for p5 <- p3.swap(i, p3 -> "max")
                                              p6 <- heapify(p5, n, p5 -> "max").!
         yield p6 else p3.!
    yield p4).get

  private def mergeSort[T: Comparable](seq: LoopableS[T, String], start: Int, end: Int):
  (LoopableS[T, String], Int, Int) = end - start match
    case n if n < 1 => (seq.divide(start, end).get, start, end)
    case n =>
      ((for p1 <- seq.divide(start, start + (n / 2))
            p2 <- mergeSort(p1, start, start + (n / 2))._1.!
            p3 <- p2.divide(start + 1 + (n / 2), end)
            p4 <- mergeSort(p3, start + 1 + (n / 2), end)._1.!
            p5 <- p4.divide(start, end)
            p6 <- merge(p5, start, start + (n / 2), end)._1.!
      yield p6).get, start, end)

  private def merge[T: Comparable](seq: LoopableS[T, String], start: Int, mid: Int, end: Int):
  (LoopableS[T, String], Int, Int) =
    ((for a1 <- seq.select("i", start)
          a2 <- a1.select("j", mid + 1)
          a3 <- for b1 <- a2.whileLoop(h => h -> "j" <= end && h -> "i" != h -> "j")
                    b2 <- b1.compare(b1 -> "j", b1 -> "i")(x => x.select("i", b1 -> "i" + 1))(x =>
                      for c1 <- for k <- x.loopFor(x -> "j" until x -> "i" by -1)
                                    d1 <- k.prev.swap(k.index, k.index - 1)
                      yield d1
                          c2 <- c1.select("i", c1 -> "i" + 1)
                          c3 <- c2.select("j", c2 -> "j" + 1)
                      yield c3)
                yield b2
          a4 <- a3.deselect("i")
          a5 <- a4.deselect("j")
    yield a5).get, start, end)

