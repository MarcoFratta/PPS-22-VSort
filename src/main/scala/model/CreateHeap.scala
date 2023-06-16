package model

import scala.annotation.tailrec

import scala.math.Ordering.Implicits._
object CreateHeap:

  private def insert[T: Ordering](heap: Seq[T], newItem: T) = {

    @annotation.tailrec
    def siftUp(h: Seq[T], idx: Int): Seq[T] = {
      val parentIdx = (idx - 1) >> 1
      if (parentIdx < 0 || h(parentIdx) > h(idx)) h
      else siftUp(h.updated(parentIdx, h(idx)).updated(idx, h(parentIdx)), parentIdx)
    }

    siftUp(heap :+ newItem, heap.length)
  }

  def heapify[T: Ordering](vs: Seq[T]): Seq[T] = vs.foldLeft(Seq.empty[T])(insert)