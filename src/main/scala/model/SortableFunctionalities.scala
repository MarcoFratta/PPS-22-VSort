package model

import scala.util.{Failure, Success, Try}
import Step.*
import model.SortOperation.SortOps
import model.SortableFunctionalities.{SortableM, Stepped}
object SortableFunctionalities:

  trait IndexableM:
    type IndexType

  class SortableM[T:Comparable](var data_ : Seq[T])
    extends IndexableM:
    override type IndexType = Int
    def data:Seq[T] = data_
    def of(seq:Seq[T]):Unit = data_ = seq
    def length: Int = data_.length

  trait Stepped[T](s: Seq[Step]):
    sortable: SortableM[T] =>
    private var steps_ : Seq[Step] = s
    def steps:Seq[Step] = steps_
    def steps_(steps: Seq[Step]): Unit = steps_ = steps


  object SortableM:
    def apply[T:Comparable](seq:Seq[T], s:Seq[Step]):
    SortableM[T] with Stepped[T] = new SortableM[T](seq) with Stepped[T](s)

object SortOperation:
  import Step.*
  trait SortOps[T]:

    def flatMap[B](f: T => SortOps[B]): SortOps[B]
    def map[B](f: T => B): SortOps[B] = flatMap(x => unit(f(x)))


  def unit[T](a: T): SortOps[T] = new SortOps[T]:
    override def flatMap[B](f: T => SortOps[B]): SortOps[B] = f(a)

  extension [T:Comparable](s:SortableM[T] with Stepped[T])
    def compare[A >: SortableM[T] with Stepped[T],B](a: Int, b: Int)(ifTrue: A => B)
                  (ifFalse: A => B): SortOps[B] =
      new SortOps[B]:
        override def flatMap[F](f: B => SortOps[F]): SortOps[F] =
          s.steps_(s.steps + Comparison(a,b))
          if summon[Comparable[T]].compare(s.data(a), s.data(b)) then f(ifTrue(s)) else f(ifFalse(s))

    def swap[A >: SortableM[T] with Stepped[T]](a: s.IndexType, b: s.IndexType): SortOps[A] =
      new SortOps[A]:
        override def flatMap[B](f: A => SortOps[B]): SortOps[B] =
          s.of(swapElements(s.data)(0,1))
          s.steps_(s.steps + Step.Swap(a, b))
          f(s)

        private def swapElements(data:Seq[T])(a: Int, b: Int): Seq[T] =
          data.updated(a, data.toList(b)).updated(b, data.toList(a))

