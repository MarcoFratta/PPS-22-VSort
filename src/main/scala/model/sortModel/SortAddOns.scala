package model.sortModel

import model.Step
import model.sortModel.SortAddOns.*
import model.sortModel.SortOperation.*
import model.sortModel.SortableFunctionalities.*

import scala.annotation.{tailrec, targetName}
import scala.language.postfixOps

object SortAddOns:

  import SortOperation.unit
  import SortableFunctionalities.*

  private def unit[T](a: T): IterateOps[T] = new IterateOps[T]:
    override def get: T = a

    override def flatMap(f: Iteration[T] => SortOps[T]): SortOps[T] = f(IterationImpl(0, a))

  trait Iteration[+T]:
    def value: Int

    def previous: T

  trait IterateOps[T]:
    def flatMap(f: Iteration[T] => SortOps[T]): SortOps[T]

    def map(f: Iteration[T] => T): SortOps[T] = flatMap(i => SortOperation.unit(f(i)))

    def get: T

  trait LoopOps[T]:
    def flatMap(f: T => SortOps[T]): SortOps[T]

    def map(f: T => T): SortOps[T] = flatMap(i => SortOperation.unit(f(i)))

    def get: T

  trait Stoppable[T]:
    sortable: SortableM[T] =>
    private var stopped_ = false

    private[sortModel] def stop: SortableM[T] with Stoppable[T] =
      this.stopped_ = true
      this

    private[sortModel] def stopped: Boolean = this.stopped_


  private case class IterationImpl[+T](override val value: Int, override val previous: T) extends Iteration[T]

  object Iteration:
    def apply[T](value: Int, prev: T): Iteration[T] =
      IterationImpl(value, prev)

  extension[T, A <: SortOps[T]] (s: A)
    @targetName("unit")
    def !! : IterateOps[T] = unit(s.get)


object Loop:

  import SortAddOns.LoopOps

  extension[T, A <: SortOps[T]] (s: A)
    @targetName("unit")
    def ?? : LoopOps[T] = new LoopOps[T]:
      override def get: T = s.get

      override def flatMap(f: T => SortOps[T]): SortOps[T] = f(s.get)


import model.sortModel.Loop.*

extension[T: Comparable, A <: SortableM[T]] (s: A)

  def loopFor(range: Range): IterateOps[A] =
    new IterateOps[A]:

      import SortAddOns.!!

      override def get: A = s

      override def flatMap(f: Iteration[A] => SortOps[A]): SortOps[A] =
        range.tail.foldLeft(f(Iteration(range.head, s)))((b, i) =>
          b.flatMap(x => f(Iteration(i, b.get))))

  def whileLoop(cond: A => Boolean): LoopOps[A] =
    new LoopOps[A]:

      import SortOperation.*

      override def get: A = s

      override def flatMap(f: A => SortOps[A]): SortOps[A] =
        @tailrec
        def loop(c: A => Boolean)(sc: A): SortOps[A] =
          if cond(sc) then loop(c)(f(sc).get) else s.!

        loop(cond)(s)

extension[T: Comparable, A <: SortableM[T] with Stoppable[T]] (s: A)
  def whileBreak(cond: A => Boolean): LoopOps[A] =
    s.whileLoop(x => cond(x) && !x.stopped)
  def break: SortOps[A] =
    s.stop
    s !

object IterableM:
  def apply[T: Comparable](seq: Seq[T]): SortableM[T] with Steps[T] with Stoppable[T] =
    new IndexableM
      with SortableM[T](seq)
      with Steps[T](Seq.empty)
      with Stoppable[T]:
      override type IndexType = Int



