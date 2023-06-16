package model

import scala.util.Try
import model.Sortable.*

trait Selectable[T] extends Sortable[T]:

  type Index = Int

  def select(s: String, a: Index): Try[Selectable[T]]

  def getSelection(s: String): Index

  def deselect(s: String): Try[Selectable[T]]

  override def swap(a: Index, b: Index): Try[Selectable[T]]

  override def compare(a: Int, b: Int)(ifTrue: Sortable[T] => Sortable[T])
    (ifFalse: Sortable[T] => Sortable[T]): Try[Selectable[T]]

object Selectable:

  def apply[T:Comparable](seq: Seq[T], step: Seq[Step], map: Map[String, Int]): Selectable[T] =
    SelectedSteppedList[T](Sortable[T](seq, step), map)
  def apply[T:Comparable](seq: Seq[T], step: Seq[Step]): Selectable[T] =
    SelectedSteppedList[T](Sortable[T](seq, step), Map.empty)
  def apply[T:Comparable](): Selectable[T] =
    SelectedSteppedList[T](Sortable[T](), Map.empty)
  def apply[T: Comparable](sortable: Sortable[T]): Selectable[T] =
    SelectedSteppedList[T](sortable, Map.empty)

private case class SelectedSteppedList[T: Comparable](list: Sortable[T], map: Map[String, Int]) extends Selectable[T]:

  override def swap(a: Int, b: Int): Try[Selectable[T]] =
    Try(Selectable(list.swap(a, b).get))

  override def select(s: String, a: Int): Try[Selectable[T]] =
    Try(Selectable(data, addStep(Step.Selection(s, validIndex(a))), addSelection(s, a)))

  def getSelection(s: String): Int = map(s)

  def deselect(s: String): Try[Selectable[T]] =
    Try(Selectable(data, addStep(Step.Deselection(s)), deleteSelection(s)))

//  override def compare(a: Index, b: Index)(ifTrue: Selectable[T] => Selectable[T])
//             (ifFalse: Selectable[T] => Selectable[T]): Try[Selectable[T]] =
//    Try {
//      val l = SelectedSteppedList(Sortable(data, addStep(Step.Comparison(a, b))), map)
//      if summon[Comparable[T]].compare(data(a), data(b)) then ifTrue(l) else ifFalse(l)
//    }

  override def compare(a: Int, b: Int)(ifTrue: Sortable[T] => Sortable[T])
             (ifFalse: Sortable[T] => Sortable[T]): Try[Selectable[T]] =
    Try(Selectable(list.compare(a, b)(ifTrue)(ifFalse).get))

  def data: Seq[T] = list.data

  def steps: Seq[Step] = list.steps

  def length(): Int = list.length()

  private def addSelection(s: String, a: Index) : Map[String, Index] =
    map updated(s, a)

  private def deleteSelection(s: String): Map[String, Index] =
    map removed s

  private def validIndex(index: Index): Int =
    if data.size > index then index else throw IllegalArgumentException()

  private def addStep(step: Step): Seq[Step] = steps :+ step
