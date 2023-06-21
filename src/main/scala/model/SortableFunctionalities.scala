package model

import model.SortOperation.{SortOps, unit}
import model.SortableFunctionalities.{IndexableM, SortableM, Steps}
import model.Step.*

import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

object SortableFunctionalities:

  trait IndexableM:
    type IndexType

  trait SortableM[T: Comparable](private var data_ : Seq[T]) extends IndexableM:
    given Conversion[SortableM[T], SortOps[SortableM[T]]] = unit(_)

    def data: Seq[T] = data_

    def withData(seq: Seq[T]): SortableM[T] =
      data_ = seq
      this

    def length: Int = data_.length


  trait Steps[T: Comparable](s: Seq[Step]) extends SortableM[T]:
    given Conversion[Steps[T], SortOps[Steps[T]]] = unit(_)

    private var steps_ : Seq[Step] = s


    def steps: Seq[Step] = steps_

    def withSteps(steps: Seq[Step]): SortableM[T] with Steps[T] =
      steps_ = steps
      this

    override def withData(seq: Seq[T]): SortableM[T] with Steps[T] =
      super.withData(seq)
      this

  trait Comparation[T, A, B, C]() extends SortableM[T]:
    private var ifTrue_ : Option[A => B] = Option.empty
    private var ifFalse_ : Option[A => C] = Option.empty

    def compare(a: IndexType, b: IndexType)(using f: Comparable[T]): Boolean

    def ifTrue(f: A => B): SortableM[T] with Comparation[T, A, B, C] =
      this.ifTrue_ = Option(f)
      this

    def ifFalse(f: A => C): SortableM[T] with Comparation[T, A, B, C] =
      this.ifFalse_ = Option(f)
      this

  object SortableM:

    def apply[T: Comparable](seq: T*):
    SortableM[T] with Steps[T] = new IndexableM
      with SortableM[T](seq)
      with Steps[T](Seq.empty):
      override type IndexType = Int


object SortOperation:
  import Step.*
  trait SortOps[T]:

    def flatMap[B](f: T => SortOps[B]): SortOps[B] = f(get)

    def map[B](f: T => B): SortOps[B] = flatMap(x => unit(f(x)))

    def get: T


  def unit[T](a: T): SortOps[T] = new SortOps[T]:
    override def get: T = a

  extension[T: Comparable, A <: SortableM[T]] (s: A)
    @targetName("unit")
    def ! : SortOps[A] = unit(s)
  extension[T: Comparable, A <: SortableM[T] with Steps[T]] (s: A)

    def compare[C <: A, D <: A](a: Int, b: Int)(ifTrue: A => SortOps[C])(ifFalse: A => SortOps[D]): SortOps[C | D] =
      new SortOps[C | D]:
        override def get: C | D =
          s.withSteps(s.steps + Comparison(a, b))
          checkBranch(a, b)(ifTrue)(ifFalse)

        private def checkBranch(a: Int, b: Int)(ifTrue: A => SortOps[C])(ifFalse: A => SortOps[D]): C | D =
          if summon[Comparable[T]].compare(s.data(a), s.data(b)) then ifTrue(s).get else ifFalse(s).get


    def swap(a: Int, b: Int): SortOps[A] =
      new SortOps[A]:
        override def get: A =
          s.withData(swapElements(s.data)(a, b)).withSteps(s.steps + Swap(a, b))
          s

        private def swapElements(data: Seq[T])(a: Int, b: Int): Seq[T] =
          data.updated(a, data.toList(b)).updated(b, data.toList(a))
  extension[T: Comparable, A <: SortableM[T]] (s: A)

    def iterate(range: Range)(g: (Int, A) => SortOps[A]): SortOps[A] =
      new SortOps[A]:
        override def get: A = range.tail.foldLeft(g(range.head, s))((b, i) => g(i, b.get)).get









