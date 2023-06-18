package model

import scala.util.{Failure, Success, Try}

import Step.*
trait Comparable[A]:
  def compare(a:A, b:A): Boolean

trait Indexable:
  type Index
class IntIndexable() extends Indexable:
  override type Index = Int
trait ComparableSortable[T] extends Indexable:
  def compare(a: Index, b: Index)(ifTrue: T => T)
                (ifFalse: T => T): Try[T]

trait Swappable[T] extends Indexable:
  def swap(a: Index, b: Index): Try[T]


trait Sortable[T]:

  def length: Int

  def data: Seq[T]

  def steps: Seq[Step]

trait DataSource[T] extends IntIndexable
  with Sortable[T]
  with Swappable[DataSource[T]]
  with ComparableSortable[DataSource[T]]

object Sortable:

  def apply[T:Comparable](seq: Seq[T], step: Seq[Step]): DataSource[T] = SteppedList(seq, step)
  def apply[T:Comparable](): DataSource[T] = SteppedList(Seq.empty, Seq.empty)
  def apply[T:Comparable](seq:T*): DataSource[T]  = SteppedList(seq, Seq.empty)


object SortableUtils:
  def genericCompare[A, K, T:Comparable](input: A)(data:Seq[T])(a: Int, b:Int )
                                                (ifTrue: A => A)(ifFalse: A => A): Try[A] =
    Try {
      if summon[Comparable[T]].compare(data(a), data(b)) then ifTrue(input) else ifFalse(input)
    }


private case class SteppedList[T: Comparable](override val data: Seq[T], override val steps: Seq[Step])
  extends DataSource[T]:
  import SortableUtils.*

  def swap(a: Index, b: Index): Try[DataSource[T]]=
    Try(SteppedList(swapElements(a, b), addStep(Step.Swap(a, b))))

  override def compare(a: Index, b: Index)(ifTrue: DataSource[T] => DataSource[T])
                            (ifFalse: DataSource[T] => DataSource[T]): Try[DataSource[T]] =
    genericCompare(SteppedList(data, addStep(Step.Comparison(a, b))))(data)(a,b)(ifTrue)(ifFalse)


  def length: Int = data.size

  private def swapElements(a: Index, b: Index): Seq[T] =
    data updated(a, data.toList(b)) updated(b, data.toList(a))

  private def addStep(step: Step): Seq[Step] = steps :+ step

  private def validIndex(index: Int): Int =
    if data.size > index then index else throw IllegalArgumentException(f"Invalid index $index")
