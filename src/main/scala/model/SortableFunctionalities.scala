package model

import scala.util.{Failure, Success, Try}
import Step.*
import model.SortOperation.SortOps
import model.SortableFunctionalities.{IndexableM, IterableM, SortableM, Stepped}
object SortableFunctionalities:

  trait IndexableM:
    type IndexType
  trait SortableM[T:Comparable](private var data_ : Seq[T]) extends IndexableM:
    def data:Seq[T] =data_

    def withData(seq: Seq[T]): SortableM[T] =
      data_ = seq;
      this
    def length: Int = data_.length



  trait Stepped[T:Comparable](s: Seq[Step]) extends SortableM[T]:
    private var steps_ : Seq[Step] = s
    def steps:Seq[Step] = steps_

    def withSteps(steps: Seq[Step]): SortableM[T] with Stepped[T]=
      steps_ = steps
      this

    override def withData(seq: Seq[T]): SortableM[T] with Stepped[T]=
      super.withData(seq); this

  trait IterableM[T](range:Range, i:Int):
    sortable:SortableM[T] =>
    override type IndexType = Int
    private var r_ = 0 to 0 by 1
    private val i_ = 0
      def iterating[B >: SortableM[T]](r: Range):B =
        r_ = r; this

      def from: IndexType = r_.start

      def to: IndexType = r_.end
      def index: IndexType = i_


  object SortableM:

    def apply[T:Comparable](seq: T*):
    SortableM[T] with Stepped[T] with IterableM[T] = new IndexableM
      with SortableM[T](seq)
      with Stepped[T](Seq.empty)
      with IterableM[T](0 to 0, 0):
      override type IndexType = Int


object SortOperation:
  import Step.*
  trait SortOps[T]:

    def flatMap[B](f: T => SortOps[B]): SortOps[B] = f(get)

    def map[B](f: T => B): SortOps[B] = flatMap(x => unit(f(x)))
    def get:T


  def unit[T](a: T): SortOps[T] = new SortOps[T]:
    override def get: T = a

  extension [T:Comparable, A <: SortableM[T] with Stepped[T]](s:A)
    def compare[C, D](a: Int, b: Int)(ifTrue: A => C)(ifFalse: A => D): SortOps[C | D] =
      new SortOps[C | D]:
        override def get: C | D =
          s.withSteps(s.steps + Comparison(a, b))
          checkBranch(a, b)(ifTrue)(ifFalse)

        private def checkBranch(a: Int, b: Int)(ifTrue: A => C )(ifFalse: A => D): C | D =
          if summon[Comparable[T]].compare(s.data(a), s.data(b)) then ifTrue(s) else ifFalse(s)


    def swap(a: Int, b: Int): SortOps[A] =
      new SortOps[A]:
        override def get: A =
          s.withData(swapElements(s.data)(a, b)).withSteps(s.steps + Swap(a, b)); s

        private def swapElements(data:Seq[T])(a: Int, b: Int): Seq[T] =
          data.updated(a, data.toList(b)).updated(b, data.toList(a))
  extension [T:Comparable, A <: SortableM[T] with IterableM[T]] (s: A)

    def iterate(range: Range)(g: (Int, A) => SortOps[A]): SortOps[A] =
      new SortOps[A]:
        override def get: A = range.tail.foldLeft(g(range.head,s))((b,i) => g(i,b.get)).get









