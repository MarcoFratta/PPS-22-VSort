package model

import scala.util.{Failure, Success, Try}


trait Sortable[T]:

  private type Index = Int

  def swap(a: Index, b: Index): Try[Sortable[T]]

  def select(s: String, a: Index): Try[Sortable[T]]

  def getSelection(s: String): Index

  def deselect(s: String): Try[Sortable[T]]

  def compare(a: Index, b: Index)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T])
             (using f: (T, T) => Boolean): Try[Sortable[T]]
  def length(): Int

  def data: Seq[T]

  def steps: Seq[Step]

object Sortable:

  def apply[T](seq: Seq[T], step: Seq[Step], map: Map[String, Int]): Sortable[T] = new SteppedList[T](seq, step, map)
  def apply[T](seq: Seq[T], step: Seq[Step]): Sortable[T] = new SteppedList[T](seq, step, Map.empty)
  def apply[T](): Sortable[T] = new SteppedList[T](Seq.empty, Seq.empty, Map.empty)
  def apply[T](seq: T*): Sortable[T] = new SteppedList[T](seq, Seq.empty, Map.empty)

private case class SteppedList[T](override val data: Seq[T], override val steps: Seq[Step], map: Map[String, Int]) extends Sortable[T]:

  def swap(a: Int, b: Int): Try[Sortable[T]] =
    Try(SteppedList(swapElements(a, b), addStep(Step.Swap(a, b)), map))

  def select(s: String, a: Int): Try[Sortable[T]] =
    Try(SteppedList(data, addStep(Step.Selection(s, validIndex(a))), addSelection(s, a)))

  def getSelection(s: String): Int = map(s)

  def deselect(s: String): Try[Sortable[T]] =
    Try(SteppedList(data, addStep(Step.Deselection(s)), deleteSelection(s)))

  def compare(a: Int, b: Int)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T])
             (using f: (T, T) => Boolean): Try[Sortable[T]] =
    Try {
      val memoryList = SteppedList(data, addStep(Step.Comparison(a, b)), map)
      if f(data(a), data(b)) then ifTrue(memoryList) else ifFalse(memoryList)
    }

  def length(): Int = data.size

  private def swapElements(a: Int, b: Int): Seq[T] =
    data updated(a, data.toList(b)) updated(b, data.toList(a))

  private def addStep(step: Step): Seq[Step] = steps :+ step

  private def addSelection(s: String, a: Int) : Map[String, Int] =
    map updated(s, a)

  private def deleteSelection(s: String): Map[String, Int] =
    map removed s

  private def validIndex(index: Int): Int =
    if data.size > index then index else throw IllegalArgumentException()
