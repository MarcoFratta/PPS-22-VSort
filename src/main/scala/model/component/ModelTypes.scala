package model.component

import model.*
import model.algorithms.SortingAlgorithms.{*, given}
import model.algorithms.{ElementInfo, StepsTransformer}
import model.api.{Comparable, Step}
import model.properties.Distributions.{GaussianDistribution, UniformDistribution}
import model.properties.{Generable, Generator}
import view.InputType

trait HasName:
  def name: String

trait State[T]:
  def get: Seq[ElementInfo[T]]


trait Distribution[K, T: Generable] extends Comparable[(Int, T)]:
  def generator(params: Map[Params, K]): Generator[T]

  def params: Set[Params]

trait Algorithm[C,R]:
  def execute(s: Seq[C]): R

trait ModelTypes:

  type ParamsType
  type ResultType
  type ValType

trait IntTypes extends ModelTypes:
  override type ParamsType = Int
  override type ValType = Int
  override type ResultType = Seq[State[Int]]


trait Algorithms extends ModelTypes:
  def algorithms: Set[Algorithm[ValType, ResultType] with HasName]

trait Distributions extends ModelTypes:
  def distributions: Set[Distribution[ParamsType, ValType] with HasName]

object DistributionFactory:


  def apply[K, T: Generable](f: Map[Params, K] => Generator[T],
                             p: Set[Params], n: String)
                            (using c: Comparable[(Int, T)]): Distribution[K, T] with HasName =
    new Distribution[K, T] with HasName:
      override def generator(params: Map[Params, K]): Generator[T] = f(params)

      override def params: Set[Params] = p

      override def compare(a: (Int, T), b: (Int, T)): Boolean = c.compare(a, b)

      override def name: String = n

  def apply[K, T: Generable](f: (Map[Params, K], Conversion[Params, K]) => Generator[T],
                             p: Set[Params], n: String,
                             c: Comparable[(Int, T)]): Distribution[K, T] with HasName =
    new Distribution[K, T] with HasName:
      override def generator(params: Map[Params, K]): Generator[T] =
        val ca: Conversion[Params, K] = x => params(x)
        f(params, ca)


      override def params: Set[Params] = p

      override def compare(a: (Int, T), b: (Int, T)): Boolean = c.compare(a, b)

      override def name: String = n



object AlgorithmFactory:
  def intAlgorithm[C:Comparable](f: Seq[C] => Seq[Step], n: String): Algorithm[C,Seq[State[C]]] with HasName =
    new Algorithm[C,Seq[State[C]]] with HasName:
      override def execute(s: Seq[C]): Seq[State[C]] =
        StepsTransformer().getSeqList(f(s), s).map(s => State(s))

      override def name: String = n

  object State:
    def apply[T](v: Seq[ElementInfo[T]]): State[T] = new State[T]:
      override def get: Seq[ElementInfo[T]] = v