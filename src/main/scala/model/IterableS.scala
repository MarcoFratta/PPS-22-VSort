package model

import model.SortableOld.*

trait IterableS[T] extends Indexable:

  def iterating(range: Range):T
  def at(index:Index):T
  def index:Int
  def from:Int
  def to:Int
  def flatMap[B](f: T => B): B
  def map[B](f: T => B):B


object IterableS:

  import SortableOld.*

  def apply[T: Comparable](seq: T*): IterableS[T] =
    IterableSortableX(SortableOld(seq, Seq.empty), 0 to seq.length by 1, 0)

private case class IterableSortableX[T :Comparable](source: DataSource[T], range: Range,
                                                    override val index:Int) extends IterableS[T]:

  export source.*


  override def at(i: Index): T = ???//IterableSortableX(source, range, i)

  override def iterating(range: Range): T = ???//IterableSortableX(source, range, index)

  override def from: Index = index

  override def flatMap[B](f: T => B): B = ???

  override def map[B](f: T => B): B = ???

  override def to: Index = to


//   def foreach(f: IterableS[T] => IterableS[T]): IterableS[T] =
//    if range.isDefinedAt(index) then f(this) at index + range.step else this




