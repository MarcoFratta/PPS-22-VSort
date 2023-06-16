package model

import Sortable.*

trait IterableS[T] extends Sortable[T]:

  override type Index = Int

  def iterating(range: Range):IterableS[T]
  def at(i:Index):IterableS[T]

  def index:Int
  def from:Int
  def to:Int

  def foreach(f: IterableS[T] => IterableS[T]): IterableS[T]


object IterableS:
  import Sortable.*
  def apply[T:Comparable](sortable: Sortable[T], range: Range,i:Int):IterableS[T] =
    IterableSortableX(sortable, range, i)

private case class IterableSortableX[T :Comparable](sortable: Sortable[T], range: Range,
                                   override val index:Int) extends IterableS[T]:

  export sortable.*

  override def at(i: Index): IterableS[T] = IterableS(sortable, range, i)

  override def iterating(range: Range): IterableS[T] = IterableS(sortable, range, index)

  override def from: Index = index

  override def to: Index = to

  override def foreach(f: IterableS[T] => IterableS[T]): IterableS[T] =
    if range.isDefinedAt(index) then f(this) at index + range.step else this



