package model.sortModel

import model.Step
import model.Step.*
import model.sortModel.SortOperation.{SortOps, unit}
import model.sortModel.SortableFunctionalities.*

import scala.annotation.targetName

object SortableFunctionalities:

  trait Comparable[A]:
    def compare(a: A, b: A): Boolean

  trait IndexableM:
    type IndexType

  trait SortableM[T: Comparable](private var data_ : Seq[T]) extends IndexableM:
    given Conversion[SortableM[T], SortOps[SortableM[T]]] = unit(_)

    def data: Seq[T] = data_

    private[sortModel] def withData(seq: Seq[T]): SortableM[T] =
      data_ = seq
      this

    def length: Int = data_.length


  trait Steps[T: Comparable](s: Seq[Step]) extends SortableM[T]:
    given Conversion[Steps[T], SortOps[Steps[T]]] = unit(_)

    private var steps_ : Seq[Step] = s


    def steps: Seq[Step] = steps_

    private[sortModel] def withSteps(steps: Seq[Step]): SortableM[T] with Steps[T] =
      steps_ = steps
      this

    override private[sortModel] def withData(seq: Seq[T]): SortableM[T] with Steps[T] =
      super.withData(seq)
      this

trait Selections[K, T] extends SortableM[T]:
  private var map_ : Map[K, IndexType] = Map.empty
  override type IndexType = Int

  def get(s: K): Option[IndexType] =
    map_.get(s)

  private[sortModel] def selected(s: K, a: IndexType): SortableM[T] with Selections[K, T] =
    map_ = map_ + ((s, a))
    this

  private[sortModel] def deselected(s: K): SortableM[T] with Selections[K, T] =
    map_ = map_.removed(s)
    this

object SortableM:
  def apply[T: Comparable](seq: Seq[T]):
  SortableM[T] with Steps[T] =
    new IndexableM
      with SortableM[T](seq)
      with Steps[T](Seq.empty):
      override type IndexType = Int

object SelectableM:
  def apply[T: Comparable](seq: Seq[T]):
  SortableM[T] with Steps[T] with Selections[String, T] =
    new IndexableM
      with SortableM[T](seq)
      with Steps[T](Seq.empty)
      with Selections[String, T]


object SortOperation:

  import Step.*

  trait SortOps[T]:

    def flatMap[B](f: T => SortOps[B]): SortOps[B] = f(get)

    def map[B](f: T => B): SortOps[B] = flatMap(x => unit(f(x)))

    def foreach(f: T => Unit): Unit = f(get)

    /** Should be only used in tests.
     *
     * @return the value contained in the monad after its application.
     */
    def get: T


  def unit[T](a: T): SortOps[T] = new SortOps[T]:
    override def get: T = a

  extension[T: Comparable, A <: SortableM[T]] (s: A)
    @targetName("unit")
    def ! : SortOps[A] = unit(s)

    def loopWhile(f: A => Boolean)(g: A => SortOps[A]): SortOps[A] =
      new SortOps[A]:
        override def get: A = if f(s) then g(s).get.loopWhile(f)(g).get else s


  extension[T: Comparable, A <: SortableM[T] with Steps[T]] (s: A)

    def compare[C <: A, D <: A](a: Int, b: Int)(ifTrue: A => SortOps[C])(ifFalse: A => SortOps[D]): SortOps[C | D] =
      new SortOps[C | D]:
        override def get: C | D =
          if s.data.isDefinedAt(a) && s.data.isDefinedAt(b) then
            s.withSteps(s.steps + Comparison(a, b))
            checkBranch(a, b)(ifTrue)(ifFalse)
          else throw new IllegalArgumentException("Invalid index")

        private def checkBranch(a: Int, b: Int)(ifTrue: A => SortOps[C])(ifFalse: A => SortOps[D]): C | D =
          if summon[Comparable[T]].compare(s.data(a), s.data(b)) then ifTrue(s).get else ifFalse(s).get


    def swap(a: Int, b: Int): SortOps[A] =
      new SortOps[A]:
        override def get: A =
          if (s.data.nonEmpty) s.withData(swapElements(s.data)(a, b)).withSteps(s.steps + Swap(a, b))
          s

        private def swapElements(data: Seq[T])(a: Int, b: Int): Seq[T] =
          data.updated(a, data.toList(b)).updated(b, data.toList(a))
  extension[T: Comparable, A <: SortableM[T]] (s: A)

    def iterate(range: Range)(g: (Int, A) => SortOps[A]): SortOps[A] =
      new SortOps[A]:
        override def get: A = range.size match
          case n if n > 1 => range.tail.foldLeft(g(range.head, s))((b, i) => g(i, b.get)).get
          case 1 => g(range.head, s).get
          case _ => s


  extension[T: Comparable, K, A <: Selections[K, T] with Steps[T]] (s: A)
    def select(k: K, i: s.IndexType): SortOps[A] =
      new SortOps[A]:
        override def get: A =
          if (s.data.nonEmpty)
            s.selected(k, i)
            s.withSteps(s.steps + Selection(k, i))
          s

    def deselect(k: K): SortOps[A] =
      new SortOps[A]:
        override def get: A =
          s.deselected(k)
          s.withSteps(s.steps + Deselection(k))
          s
    def getSelection(k: K): SortOps[Option[s.IndexType]] =
      new SortOps[Option[s.IndexType]]:
        override def get: Option[s.IndexType] =
          s.get(k)

    @targetName("Alias for getSelection of Selection")
    def ->(k: K): s.IndexType = s.get(k).get













