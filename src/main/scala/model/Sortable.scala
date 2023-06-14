package model

import scala.util.{Failure, Success, Try}


trait Comparable[A]:
  def compare(a:A,b:A):Boolean

trait Sortable[T]:

  type Index = Int

  def swap(a: Index, b: Index): Try[Sortable[T]]

  def select(s: String, a: Index): Try[Sortable[T]]

  def getSelection(s: String): Index

  def deselect(s: String): Try[Sortable[T]]

  def compare(a: Index, b: Index)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T]): Try[Sortable[T]]
  def length(): Int

  def data: Seq[T]

  def steps: Seq[Step]

object Sortable:

  def apply[T:Comparable](seq: Seq[T], step: Seq[Step], map: Map[String, Int]): Sortable[T] = SteppedList[T](seq, step, map)
  def apply[T:Comparable](seq: Seq[T], step: Seq[Step]): Sortable[T] = SteppedList[T](seq, step, Map.empty)
  def apply[T:Comparable](): Sortable[T] = SteppedList[T](Seq.empty, Seq.empty, Map.empty)
  def apply[T:Comparable](seq: T*): Sortable[T] = SteppedList[T](seq, Seq.empty, Map.empty)

private case class SteppedList[T: Comparable](override val data: Seq[T], override val steps: Seq[Step],
                                              map: Map[String, Int]) extends Sortable[T]:

  def swap(a: Int, b: Int): Try[Sortable[T]] =
    Try(SteppedList(swapElements(a, b), addStep(Step.Swap(a, b)), map))

  def select(s: String, a: Int): Try[Sortable[T]] =
    Try(SteppedList(data, addStep(Step.Selection(s, validIndex(a))), addSelection(s, a)))

  def getSelection(s: String): Int = map(s)

  def deselect(s: String): Try[Sortable[T]] =
    Try(SteppedList(data, addStep(Step.Deselection(s)), deleteSelection(s)))

  def compare(a: Index, b: Index)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T]): Try[Sortable[T]] =
    Try {
      val l = SteppedList(data, addStep(Step.Comparison(a, b)), map)
      if summon[Comparable[T]].compare(data(a), data(b)) then ifTrue(l) else ifFalse(l)
    }

  def length(): Int = data.size

  private def swapElements(a: Index, b: Index): Seq[T] =
    data updated(a, data.toList(b)) updated(b, data.toList(a))

  private def addStep(step: Step): Seq[Step] = steps :+ step

  private def addSelection(s: String, a: Index) : Map[String, Index] =
    map updated(s, a)

  private def deleteSelection(s: String): Map[String, Index] =
    map removed s

  private def validIndex(index: Index): Int =
    if data.size > index then index else throw IllegalArgumentException()
