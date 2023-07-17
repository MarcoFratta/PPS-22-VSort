package model.sortModel

import model.Step
import model.sortModel.SortAddOns.*
import model.sortModel.Sortable.*
import model.sortModel.SortableOps.{!, Monad}

import scala.annotation.{tailrec, targetName}
import scala.language.postfixOps

object SortAddOns:

  private def unit[T](a: T): IterateM[T] = new IterateM[T]:
    override def get: T = a

    override def flatMap(f: Iteration[T] => Monad[T]): Monad[T] = f(IterationImpl(0, a))

  trait Iteration[+T]:
    def index: Int

    def prev: T

  trait IterateM[T]:
    def flatMap(f: Iteration[T] => Monad[T]): Monad[T]

    def map(f: Iteration[T] => T): Monad[T] = flatMap(i => SortableOps.unit(f(i)))

    def get: T

  trait LoopM[T]:
    def flatMap(f: T => Monad[T]): Monad[T]

    def map(f: T => T): Monad[T] = flatMap(i => SortableOps.unit(f(i)))

    def get: T

  trait Stoppale[C](b: Boolean):

    private[sortModel] def stopped: C

    private[sortModel] def isStopped: Boolean = b


  private case class IterationImpl[+T](override val index: Int, override val prev: T) extends Iteration[T]

  object Iteration:
    def apply[T](value: Int, prev: T): Iteration[T] =
      IterationImpl(value, prev)

  extension[T, A <: Monad[T]] (s: A)
    @targetName("unit")
    def !! : IterateM[T] = unit(s.get)


object LoopOperation:
  extension[T, A <: Monad[T]] (s: A)
    @targetName("unit")
    def ?? : LoopM[T] = new LoopM[T]:
      override def get: T = s.get

      override def flatMap(f: T => Monad[T]): Monad[T] = f(s.get)

  extension[T: Comparable, A] (s: A)

    def loopFor(range: Range): IterateM[A] =
      new IterateM[A]:

        override def get: A = s

        override def flatMap(f: Iteration[A] => Monad[A]): Monad[A] =
          range.tail.foldLeft(f(Iteration(range.head, s)))((b, i) =>
            b.flatMap(x => f(Iteration(i, x))))

    def whileLoop(cond: A => Boolean): LoopM[A] =
      new LoopM[A]:

        override def get: A = s

        override def flatMap(f: A => Monad[A]): Monad[A] =
          @tailrec
          def loop(c: A => Boolean)(sc: A): Monad[A] =
            if cond(sc) then loop(c)(f(sc).get) else sc.!

          loop(cond)(s)


  extension[T: Comparable, A <: Stoppale[A]] (s: A)

    def whileBreak(cond: A => Boolean): LoopM[A] =
      s.whileLoop(x => cond(x) && !x.isStopped)

    def break: Monad[A] = s.stopped.!

object Loopable:
  def apply[T: Comparable, V](seq: Seq[T]):
  LoopableS[T, V] = new LoopableS[T, V](seq, Seq.empty, Map.empty, false)

  case class LoopableS[T, K](override val data: Seq[T], override val steps: Seq[Step],
                             override val selections: Map[K, Int], b: Boolean)
    extends Steppable[LoopableS[T, K]]
      with Data[T, LoopableS[T, K]]
      with Selections[K, Int, LoopableS[T, K]]
      with Stoppale[LoopableS[T, K]](b):
    override private[sortModel] def ofSteps(st: Seq[Step]) = this.copy(steps = st)

    override private[sortModel] def ofSelections(mp: Map[K, Int]) = this.copy(selections = mp)

    override private[sortModel] def ofData(dt: Seq[T]) = this.copy(data = dt)

    override private[sortModel] def stopped = this.copy(b = true)



