package model

import model.Selectable.Key
import model.Sortable.*

import scala.util.Try

trait Selectable[K, T] extends Sortable[T]:

  def select(s: K, a: Index): Try[Selectable[K, T]]

  def getSelection(s: K): Index

  def deselect(s: K): Try[Selectable[K, T]]

object Selectable:

  import Sortable.*
  type Key = String

  def apply[T: Comparable](): Selectable[Key, T] =
    SelectedSteppedList[Key, T](Sortable[T](), Map.empty)

  def apply[T: Comparable](seq: T*): Selectable[Key, T] =
    SelectedSteppedList[Key, T](Sortable(seq, Seq.empty), Map.empty)

private case class SelectedSteppedList[K, T: Comparable](list: Sortable[T], map: Map[K, Int])
  extends Selectable[K, T]:
  import SortableUtils.*

  override type Index = Int

  export list.{steps, data, length}

  override def swap(a: Index, b: Index): Try[Sortable[T]] =
    list.swap(a,b).map(v => SelectedSteppedList(v,map))

  override def compare[A >: Selectable[K, T], B](a: Index, b: Index)(ifTrue: A => B)(ifFalse: A => B): Try[B] =
    genericCompare(SelectedSteppedList(Sortable(data, addStep(Step.Comparison(a, b))), map))(data)(a, b)(ifTrue)(ifFalse)

  override def select(s: K, a: Int): Try[Selectable[K, T]] =
    Try(SelectedSteppedList(Sortable(data, addStep(Step.Selection(s, validIndex(a)))), addSelection(s, a)))

  def getSelection(s: K): Int = map(s)

  def deselect(s: K): Try[Selectable[K, T]] =
    Try(SelectedSteppedList(Sortable(data, addStep(Step.Deselection(s))), deleteSelection(s)))

  private def deleteSelection(s: K): Map[K, Index] = map removed s

  private def addStep(step: Step): Seq[Step] = steps :+ step

  private def addSelection(s: K, a: Index): Map[K, Index] =
    map updated(s, a)

  private def validIndex(index: Index): Int =
    if data.size > index then index else throw IllegalArgumentException()