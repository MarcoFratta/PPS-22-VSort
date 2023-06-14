package model

import scala.annotation.tailrec

object CreateHeap:
    def buildHeap(seq: Seq[Int]): Seq[Int] =
      val size = seq.size
      val start = (size / 2) - 1
      (start to 0 by -1).foldLeft(seq) ( (acc, i) => heapify(acc, i, size))

    @tailrec
    private def heapify(seq: Seq[Int], i: Int, size: Int): Seq[Int] =
      val left = 2 * i + 1
      val right = 2 * i + 2
      var largest = i

      if (left < size && seq(left) > seq(largest))
        largest = left

      if (right < size && seq(right) > seq(largest))
        largest = right

      if (largest != i) {
        val updatedSeq = swap(seq, i, largest)
        heapify(updatedSeq, largest, size)
      } else {
        seq
      }

    private def swap(seq:Seq[Int], i: Int, j:Int):Seq[Int] =
      seq.updated(i, seq(j)).updated(j, seq(i))
