package model

import scala.util.{Failure, Success, Try}

import Step.*
trait Comparable[A]:
  def compare(a:A, b:A): Boolean

trait Sortable[T]:

  type Index = Int

  def swap(a: Index, b: Index): Try[Sortable[T]]

  def compare[A >: Sortable[T],B <: Sortable[T]](a: Index, b: Index)(ifTrue: A => B)
                   (ifFalse: A => B): Try[B]

  def length: Int

  def data: Seq[T]

  def steps: Seq[Step]

object Sortable:

  def apply[T:Comparable](seq: Seq[T], step: Seq[Step]): Sortable[T] = SteppedList(seq, step)
  def apply[T:Comparable](): Sortable[T] = SteppedList(Seq.empty, Seq.empty)

  def apply[T:Comparable](seq:T*): Sortable[T]  = SteppedList(seq, Seq.empty)


object SortableUtils:
  def genericCompare[A, B, T:Comparable](input: A)(data:Seq[T])(a: Int, b: Int)
                                                (ifTrue: A => B)(ifFalse: A => B): Try[B] =
    Try {
      if summon[Comparable[T]].compare(data(a), data(b)) then ifTrue(input) else ifFalse(input)
    }



private case class SteppedList[T: Comparable](override val data: Seq[T], override val steps: Seq[Step])
  extends Sortable[T]:
  import SortableUtils.*

  def swap(a: Int, b: Int): Try[Sortable[T]]=
    Try(SteppedList(swapElements(a, b), addStep(Step.Swap(a, b))))

  override def compare[A >: Sortable[T], B <: Sortable[T]](a: Index, b: Index)(ifTrue: A => B)
                            (ifFalse: A => B): Try[B] =
    genericCompare(SteppedList(data, addStep(Step.Comparison(a, b))))(data)(a,b)(ifTrue)(ifFalse)


  def length: Int = data.size

  private def swapElements(a: Index, b: Index): Seq[T] =
    data updated(a, data.toList(b)) updated(b, data.toList(a))

  private def addStep(step: Step): Seq[Step] = steps :+ step

  private def validIndex(index: Int): Int =
    if data.size > index then index else throw IllegalArgumentException(f"Invalid index $index")
