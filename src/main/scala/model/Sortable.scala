package model

import scala.util.{Failure, Success, Try}


trait Sortable[T]:
  def swap(a: Int, b: Int): Try[Sortable[T]]
  
  def select(a: Int): Try[Sortable[T]]

  def deselect(a: Int): Try[Sortable[T]]

  def compare(a: Int, b: Int)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T])
             (using f: (T, T) => Boolean): Try[Sortable[T]]
  def length(): Int
object Sortable:
  def apply[T](seq: Seq[T], step: Seq[Step]): Sortable[T] = new StepList[T](seq, step)
  def apply[T](): Sortable[T] = new StepList[T](Seq.empty, Seq.empty)
  def apply[T](seq:T*): Sortable[T] = new StepList[T](seq, Seq.empty)

private case class StepList[T](data: Seq[T], steps: Seq[Step]) extends Sortable[T]:

  def swap(a: Int, b: Int): Try[Sortable[T]] =
    Try(StepList(swapElements(a, b), addStep(Step.Swap(a, b))))

  def select(a: Int): Try[Sortable[T]]=
    Try(StepList(data, addStep(Step.Selection(a))))

  def deselect(a: Int): Try[Sortable[T]] =
    Try(StepList(data, addStep(Step.Deselection(a))))

  def compare(a: Int, b: Int)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T])
             (using f: (T, T) => Boolean): Try[Sortable[T]] =
    Try {
      val memoryList = StepList(data, addStep(Step.Comparison(a, b)))
      if f(data(a), data(b)) then ifTrue(memoryList) else ifFalse(memoryList)
    }

  def length(): Int = data.size

  private def swapElements(a: Int, b: Int): Seq[T] =
    data updated(a, data.toList(b)) updated(b, data.toList(a))

  private def addStep(step: Step): Seq[Step] = steps :+ step
