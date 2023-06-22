package model

import model.Step.Comparison
import model.sortModel.SortOperation.*
import model.sortModel.SortableFunctionalities.*
import model.sortModel.SortableM

import scala.language.postfixOps

object SortingAlgorithms {

  given Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  given Conversion[Steps[Int], SortOps[Steps[Int]]] = _ !

  def bubbleSort(seq: Seq[Int]): Seq[Step] =
    (for res <- SortableM(seq).iterate(0 to seq.length - 2)(
      (i, t) => t.iterate(0 to t.length - 2 - i)(
        (j, t2) => t2.compare(j, j + 1)(x => x.swap(j, j + 1))(x => x)))
    yield (res.data, res.steps)).get._2


  def selectionSort(seq: Seq[Int]): Seq[Step] = ???

  def insertionSort(seq: Seq[Int]): Seq[Step] = ???

  def mergeSort(seq: Seq[Int]): Seq[Step] = ???



  def mergesort(seq: List[Int]): List[Int] = seq.length match
    case n if n < 2 => seq
    case _ =>
      val l = seq.splitAt(seq.length/2)._1
      val r = seq.splitAt(seq.length/2)._2
      merge(mergesort(l), mergesort(r))

  private def merge(l: List[Int], r: List[Int]): List[Int] =
    var list = l.concat(r)


    var i = 0
    var j = l.length

    while (j < list.length) {
      if list(i) < list(j) then
        i = i + 1
      else
        for (k <- j until i by -1) {
          list = list.updated(k, list(k - 1)).updated(k - 1, list(k))
        }
        i = i + 1
        j = j + 1
    }

    list

}
