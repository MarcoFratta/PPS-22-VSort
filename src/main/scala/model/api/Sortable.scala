package model.api

import model.api.Step.*

import scala.annotation.targetName

trait Comparable[T]:
  def compare(a: T, b: T): Boolean


trait Steppable[+C]:
  def steps: Seq[Step]

  private[api] def ofSteps(st: Seq[Step]): C

trait Data[T, +C]:
  def data: Seq[T]

  private[api] def ofData(dt: Seq[T]): C

trait Selections[K, V, +C]:
  def selections: Map[K, V]

  def get(s: K): Option[V] = selections.get(s)

  private[api] def ofSelections(d: Map[K, V]): C


object Sortable:
  def apply[T: Comparable, V](seq: Seq[T]):
  Sortable[T, V] = new Sortable[T, V](seq.toVector, Vector.empty, Map.empty)

  case class Sortable[T, K](override val data: Seq[T], override val steps: Seq[Step],
                            override val selections: Map[K, Int])
    extends Steppable[Sortable[T, K]]
      with Data[T, Sortable[T, K]]
      with Selections[K, Int, Sortable[T, K]]:
    override private[api] def ofSteps(st: Seq[Step]): Sortable[T, K] = this.copy(steps = st)

    override private[api] def ofSelections(mp: Map[K, Int]) = this.copy(selections = mp)

    override private[api] def ofData(dt: Seq[T]) = this.copy(data = dt)


object SortableOps:

  import Step.*

  extension[A] (s: A) 
    @targetName("Monad unit")
    def ! : Monad[A] = unit(s)

  def unit[T](a: T): Monad[T] = new Monad[T]:
    override def get: T = a

  trait Monad[T]:

    def map[B](f: T => B): Monad[B] = flatMap(x => unit(f(x)))

    def flatMap[B](f: T => Monad[B]): Monad[B] = f(get)

    def foreach(f: T => Unit): Unit = f(get)

    /** Should be only used in tests.
     *
     * @return the value contained in the monad after its application.
     */
    def get: T

  extension[T: Comparable, A <: Steppable[A] with Data[T, A]] (s: A)
    def swap(a: Int, b: Int): Monad[A] =
      new Monad[A]:
        override def get: A =
          if s.data.nonEmpty then s.ofData(swapElements(s.data)(a, b)).ofSteps(s.steps :+ Swap(a, b))
          else s

        private def swapElements(data: Seq[T])(a: Int, b: Int): Seq[T] =
          data.updated(a, data.toList(b)).updated(b, data.toList(a))


    def compare[C <: A, D <: A](a: Int, b: Int)(ifTrue: A => Monad[C])(ifFalse: A => Monad[D]): Monad[C | D] =
      new Monad[C | D]:
        override def get: C | D =
          if s.data.isDefinedAt(a) && s.data.isDefinedAt(b) then
            checkBranch(s.ofSteps(s.steps :+ Comparison(a, b)))(a, b)(ifTrue)(ifFalse)
          else throw new IllegalArgumentException(f"Invalid compare indexes ($a - $b)")

        private def checkBranch(d: A)(a: Int, b: Int)(ifTrue: A => Monad[C])(ifFalse: A => Monad[D]): C | D =
          if summon[Comparable[T]].compare(d.data(a), d.data(b)) then ifTrue(d).get else ifFalse(d).get

    def divide(a: Int, b: Int): Monad[A] =
      new Monad[A]:
        override def get: A = if s.data.isDefinedAt(a) && s.data.isDefinedAt(b) then
          s.ofSteps(s.steps :+ Divide(a, b))
        else throw new IllegalArgumentException(f"Invalid divide indexes ($a - $b)")


  extension[T: Comparable, K, V, A <: Steppable[A] with Data[T, A] with Selections[K, V, A]] (s: A)
    def select(k: K, i: V): Monad[A] =
      new Monad[A]:
        override def get: A =
          s.ofSelections(s.selections.updated(k, i)).ofSteps(s.steps :+ Selection(k, i))
  extension[T: Comparable, K, V, A <: Steppable[A] with Selections[K, V, A]] (s: A)
    def deselect(k: K): Monad[A] =
      new Monad[A]:
        override def get: A =
          s.ofSelections(s.selections.removed(k)).ofSteps(s.steps :+ Deselection(k))
    def getSelection(k: K): Monad[Option[V]] =
      new Monad[Option[V]]:
        override def get: Option[V] = s.get(k)

    @targetName("Alias for getSelection of Selection")
    def ->(k: K): V = s.get(k).get















