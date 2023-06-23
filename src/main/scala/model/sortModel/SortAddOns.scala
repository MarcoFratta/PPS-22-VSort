package model.sortModel

import model.Step
import model.sortModel.SortAddOns.{IterateOps, Iteration}
import model.sortModel.SortOperation.{SortOps, unit}
import model.sortModel.SortableFunctionalities.{Comparable, SortableM, Steps}

import scala.annotation.targetName
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

  private case class IterationImpl[+T](override val value: Int, override val previous: T) extends Iteration[T]

  object Iteration:
    def apply[T](value: Int, prev: T): Iteration[T] =
      IterationImpl(value, prev)

  extension[T, A <: SortOps[T]] (s: A)
    @targetName("unit")
    def !! : IterateOps[T] = unit(s.get)

extension[T: Comparable, A <: SortableM[T]] (s: A)
  def loopFor(range: Range): IterateOps[A] =
    new IterateOps[A]:

      import SortAddOns.!!

      override def get: A = s

      override def flatMap(f: Iteration[A] => SortOps[A]): SortOps[A] =
        range.tail.foldLeft(f(Iteration(range.head, s)))((b, i) =>
          b.flatMap(x => f(Iteration(i, b.get))))


extension[T: Comparable, A <: SortableM[T] with Steps[T]] (s: A)

  def compareAfter[C <: A, D <: A](a: => Int, b: => Int)(ifTrue: A => IterateOps[C])(ifFalse: A => IterateOps[D]): IterateOps[C | D] =
    new IterateOps[C | D]:

      import SortAddOns.!!

      override def get: C | D =
        if s.data.isDefinedAt(a) && s.data.isDefinedAt(b) then
          s.withSteps(s.steps + Step.Comparison(a, b))
          checkBranch(a, b)(ifTrue)(ifFalse)
        else throw new IllegalArgumentException(f"Invalid compare indexes ($a - $b)")

      override def flatMap(f: Iteration[C | D] => SortOps[C | D]): SortOps[C | D] = f(Iteration(0, get))

      private def checkBranch(a: Int, b: Int)(ifTrue: A => IterateOps[C])(ifFalse: A => IterateOps[D]): C | D =
        if summon[Comparable[T]].compare(s.data(a), s.data(b)) then ifTrue(s).get else ifFalse(s).get


