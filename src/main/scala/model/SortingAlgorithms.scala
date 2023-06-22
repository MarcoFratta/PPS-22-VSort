package model

import model.Step.Comparison
import model.sortModel.{SelectableM, Selections}
import model.sortModel.SortOperation.*
import model.sortModel.SortableFunctionalities.*
import model.sortModel.SortableM

import scala.language.postfixOps

object SortingAlgorithms {

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
//
//  def mergeSort(seq: Seq[Int]): Seq[Step] = ???


  def mergesort(seq: List[Int], start: Int, end: Int): (List[Int], Int, Int) = end - start match
    case n if n < 1 => (seq, start, end)
    case n => merge(mergesort(seq, start, start + (n/2)), mergesort(seq, start + 1 + (n/2), end))

  private def merge(l: (List[Int], Int, Int), r: (List[Int], Int, Int)): (List[Int], Int, Int) =

    val startl = l._2
    val startr = r._2
    val end = r._3
    var list = l._1.zipWithIndex.map((v, i) => i match
      case n if n >= startr => r._1(n)
      case _ => v)

    println("metto in ordine da " + startl + " a " + end)

    var i = startl
    var j = startr

    while (j < end + 1) {
      if list(i) < list(j) then
        i = i + 1
      else
        for (k <- j until i by -1) {
          println("swappare tra " + (k-1) + " e " + k)
          list = list.updated(k, list(k-1)).updated(k-1, list(k))
        }
        i = i + 1
        j = j + 1
    }

    println("messo in ordine da " + startl + " a " + end)

    (list, startl, end)
}
