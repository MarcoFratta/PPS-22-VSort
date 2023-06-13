package model

import scala.util.{Failure, Success, Try}


trait Sortable[T]:

  private type Index = Int

  def swap(a: Index, b: Index): Try[Sortable[T]]

  def select(a: Index): Try[Sortable[T]]

  def deselect(a: Index): Try[Sortable[T]]

  def compare(a: Index, b: Index)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T])
             (using f: (T, T) => Boolean): Try[Sortable[T]]
  def length(): Int

  def data: Seq[T]

  def steps: Seq[Step]

object Sortable:

  def apply[T](seq: Seq[T], step: Seq[Step]): Sortable[T] = new SteppedList[T](seq, step)
  def apply[T](): Sortable[T] = new SteppedList[T](Seq.empty, Seq.empty)
  def apply[T](seq:T*): Sortable[T] = new SteppedList[T](seq, Seq.empty)

private case class SteppedList[T](override val data: Seq[T], override val steps: Seq[Step]) extends Sortable[T]:

  def swap(a: Int, b: Int): Try[Sortable[T]] =
    Try(SteppedList(swapElements(a, b), addStep(Step.Swap(a, b))))
  def select(a: Int): Try[Sortable[T]] =
    Try(SteppedList(data, addStep(Step.Selection(validIndex(a)))))

  def deselect(a: Int): Try[Sortable[T]] =
    Try(SteppedList(data, addStep(Step.Deselection(validIndex(a)))))

  def compare(a: Int, b: Int)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T])
             (using f: (T, T) => Boolean): Try[Sortable[T]] =
    Try {
      val memoryList = SteppedList(data, addStep(Step.Comparison(a, b)))
      if f(data(a), data(b)) then ifTrue(memoryList) else ifFalse(memoryList)
    }

  def length(): Int = data.size

  private def swapElements(a: Int, b: Int): Seq[T] =
    data updated(a, data.toList(b)) updated(b, data.toList(a))

  private def addStep(step: Step): Seq[Step] = steps :+ step

  private def validIndex(index: Int): Int =
    if data.size > index then index else throw IllegalArgumentException()
