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
                  l1 <- j.previous.compare(j.previous -> "min", j.value)(x =>
                    x.select("min", j.value))(x => x)
              yield l1
         min <- p2.getSelection("min")
         p3 <- p2.deselect("min")
         p4 <- p3.swap(min.get, i.value)
      yield p4).get.steps

  def insertionSort(seq: Seq[Int]): Seq[Step] =

    given Comparable[Int] with
      override def compare(a: Int, b: Int): Boolean = a - b > 0
    given Conversion[Steps[Int] with Selections[String, Int], SortOps[Steps[Int] with Selections[String, Int]]] = _ !

    (for i <- SelectableM(seq).loopFor(1 until seq.length)
         p1 <- i.previous.select("sel", i.value)
         p2 <- for j <- p1.loopFor(i.value - 1 to 0 by -1)
                  l1 <- j.previous.compare(j.value, j.previous -> "sel")(x =>
                    for ll1 <- x.swap(x -> "sel", j.value)
                        ll2 <- ll1.select("sel", j.value)
                    yield ll2)(x => x) // x => x con break
                yield l1
         p3 <- p2.deselect("sel")
    yield p3).get.steps

  def heapSort(seq: Seq[Int]): Seq[Step] =
    (for p1 <- for i <- SelectableM(seq).loopFor(seq.length / 2 - 1 to 0 by -1)
                  l1 <- heapify(i.previous, seq.length, i.value).!
               yield l1
         p2 <- for i <- p1.loopFor(p1.length - 1 to 0 by -1)
                  l1 <- i.previous.swap(i.value, 0)
                  l2 <- heapify(l1, i.value, 0).!
               yield l2
    yield p2).get.steps

  def heapify[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], n: Int, i: Int):
  SortableM[T] with Steps[T] with Selections[String, T] =
    (for p1 <- seq.select("max", i)
         p2 <- if 2*i+1 < n then p1.compare(p1 -> "max", 2*i+1)(x => x.select("max", 2*i+1))(x => x.!) else p1.!
         p3 <- if 2*i+2 < n then p2.compare(p2 -> "max", 2*i+2)(x => x.select("max", 2*i+2))(x => x.!) else p2.!
         p4 <- if i != (p3 -> "max") then for p5 <- p3.swap(i, p3 -> "max")
                                              p6 <- heapify(p5, n, p5 -> "max").!
                                          yield p6 else p3.!
      yield p4).get

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = b - a > 0

  def mergeSort(seq: Seq[Int]): Seq[Step] = mergeSort(SelectableM(seq), 0, seq.length - 1)._1.steps

  private def mergeSort[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) = end - start match
    case n if n < 1 => ((for p1 <- seq.divide(start, end) yield p1).get, start, end)
    case n =>
      ((for p1 <- seq.divide(start, start + (n / 2))
            p2 <- mergeSort(p1, start, start + (n / 2))._1.!
            p3 <- p2.divide(start + 1 + (n / 2), end)
            p4 <- mergeSort(p3, start + 1 + (n / 2), end)._1.!
            p5 <- p4.divide(start, end)
            p6 <- merge(p5, start, start + (n / 2), end)._1.!
      yield p6).get, start, end)

  private def merge[T: Comparable](seq: SortableM[T] with Steps[T] with Selections[String, T], start: Int, mid: Int, end: Int):
  (SortableM[T] with Steps[T] with Selections[String, T], Int, Int) =
    ((for a1 <- seq.select("i", start)
          a2 <- a1.select("j", mid + 1)
          a3 <- for b1 <- a2.whileLoop(h => h -> "j" < end + 1 && h -> "i" != h -> "j")
                    b2 <- b1.compare(b1 -> "i", b1 -> "j")(x => x.select("i", b1 -> "i" + 1))(x =>
                        for c1 <- for k <- x.loopFor(x -> "j" until x -> "i" by -1)
                                      d1 <- k.previous.swap(k.value, k.value - 1)
                                  yield d1
                            c2 <- c1.select("i", c1 -> "i" + 1)
                            c3 <- c2.select("j", c2 -> "j" + 1)
                        yield c3)
                yield b2
          a4 <- a3.deselect("i")
          a5 <- a4.deselect("j")
    yield a5).get, start, end)

