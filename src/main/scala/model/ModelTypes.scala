package model

import model.SortingAlgorithms.{*, given}
import model.{InputType, StepsTransformer}
import model.seqProperties.{Generable, Generator}
import model.seqProperties.Distributions.{GaussianDistribution, UniformDistribution}
import model.sortModel.Comparable

trait HasName:
  def name: String

trait State[T]:
  def get: Seq[ElementInfo[T]]


trait Distribution[K, T: Generable]:
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

trait Model extends Algorithms with Distributions


object DistributionFactory:
  def apply[K, T: Generable](f: Map[Params, K] => Generator[T], p: Set[Params], n: String): Distribution[K, T] with HasName =
    new Distribution[K, T] with HasName:
      override def generator(params: Map[Params, K]): Generator[T] = f(params)

      override def params: Set[Params] = p

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


case class IntModelImpl() extends Model with IntTypes:
  import model.Params.*
  export model.Params.*
  given Generable[Int] = x => x.toInt

  override def algorithms: Set[Algorithm[ValType, ResultType] with HasName] = ???

  override def distributions: Set[Distribution[ParamsType, ValType] with HasName] = ???