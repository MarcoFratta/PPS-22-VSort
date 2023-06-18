package model

import model.Selectable.{Key, SelectableDataSource}
import model.Sortable.*

import scala.util.Try

trait Selectable[K,O] extends Indexable:

  def select(s: K, a: Index): Try[O]

  def getSelection(s: K): Index

  def deselect(s: K): Try[O]

object Selectable:

  import Sortable.*
  type Key = String

  trait SelectableDataSource[K,T] extends IntIndexable
    with Sortable[T]
    with ComparableSortable[SelectableDataSource[K,T]]
    with Swappable[SelectableDataSource[K,T]]
    with Selectable[K,SelectableDataSource[K,T]]


  def apply[T: Comparable]():SelectableDataSource[Key,T] =
    SelectedSteppedList[Key, T](Sortable[T](), Map.empty)

  def apply[T: Comparable](seq: T*): SelectableDataSource[Key, T] =
      SelectedSteppedList[Key, T](Sortable(seq, Seq.empty), Map.empty)

private case class SelectedSteppedList[K,T: Comparable](list: DataSource[T], map: Map[K,Int])
  extends SelectableDataSource[K,T]:
  import SortableUtils.*

  export list.{length, steps, data}

  override def swap(a: Index, b: Index): Try[SelectableDataSource[K,T]] =
    list.swap(a,b).map(v => SelectedSteppedList(v,map))

  override def compare(a: Index, b: Index)(ifTrue: SelectableDataSource[K,T] => SelectableDataSource[K,T])
                         (ifFalse: SelectableDataSource[K,T] => SelectableDataSource[K,T]): Try[SelectableDataSource[K,T]] =
    genericCompare(SelectedSteppedList(Sortable(data, addStep(Step.Comparison(a, b))),map))(data)(a, b)(ifTrue)(ifFalse)


  override def select(s: K, a: Index): Try[SelectableDataSource[K, T]] =
    Try(SelectedSteppedList(Sortable(data, addStep(Step.Selection(s, validIndex(a)))), addSelection(s, a)))

  def getSelection(s: K): Index = map(s)

  def deselect(s: K): Try[SelectableDataSource[K, T]] =
    Try(SelectedSteppedList(Sortable(data, addStep(Step.Deselection(s))), deleteSelection(s)))

  private def deleteSelection(s: K): Map[K, Index] = map removed s

  private def addStep(step: Step): Seq[Step] = steps :+ step

  private def addSelection(s: K, a: Index): Map[K, Index] = map updated(s, a)


  private def validIndex(index: Index): Int =
    if data.size > index then index else throw IllegalArgumentException()