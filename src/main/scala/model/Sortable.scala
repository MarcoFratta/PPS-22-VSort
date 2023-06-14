package model

import scala.util.{Failure, Success, Try}


trait Comparable[A]:
  def compare(a:A, b:A): Boolean

trait Sortable[T]:

  type Index = Int

  def swap(a: Index, b: Index): Try[Sortable[T]]

  def compare(a: Index, b: Index)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T]): Try[Sortable[T]]
  def length(): Int

  def data: Seq[T]

  def steps: Seq[Step]

object Sortable:

  def apply[T:Comparable](seq: Seq[T], step: Seq[Step]): Sortable[T] = SteppedList[T](seq, step)
  def apply[T:Comparable](): Sortable[T] = SteppedList[T](Seq.empty, Seq.empty)
  def apply[T:Comparable](seq: T*): Sortable[T] = SteppedList[T](seq, Seq.empty)

private case class SteppedList[T: Comparable](override val data: Seq[T], override val steps: Seq[Step]) extends Sortable[T]:

  def swap(a: Int, b: Int): Try[Sortable[T]] =
    Try(SteppedList(swapElements(a, b), addStep(Step.Swap(a, b))))

  def compare(a: Index, b: Index)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T]): Try[Sortable[T]] =
    Try {
      val l = SteppedList(data, addStep(Step.Comparison(a, b)))
      if summon[Comparable[T]].compare(data(a), data(b)) then ifTrue(l) else ifFalse(l)
    }

  def length(): Int = data.size

  private def swapElements(a: Index, b: Index): Seq[T] =
    data updated(a, data.toList(b)) updated(b, data.toList(a))

  private def addStep(step: Step): Seq[Step] = steps :+ step