package model

import model.SortingAlgorithms.{*, given}
import model.{InputType, StepsTransformer}
import model.seqProperties.{Generable, Generator}
import model.seqProperties.Distributions.{GaussianDistribution, UniformDistribution}

trait HasName:
  def name: String

trait State[T]:
  def get: Seq[T]

trait Distribution[K, T: Generable]:
  def generator(params: Map[Params, K]): Generator[T]

  def params: Set[Params]

trait Algorithm[G, R]:
  def execute(s: Seq[G]): R

trait Model:

  type ParamsType
  type ResultType
  type ValType

trait IntModel extends Model:
  override type ParamsType = Int
  override type ValType = Int

trait StateResult extends Model:
  override type ResultType = Seq[State[ElementInfo[ValType]]]

trait Algorithms extends Model:
  def algorithms: Set[Algorithm[ValType, ResultType] with HasName]

trait Distributions extends Model:
  def distributions: Set[Distribution[ParamsType, ValType] with HasName]


object DistributionFactory:
  def apply[K, T: Generable](f: Map[Params, K] => Generator[T], p: Set[Params], n: String): Distribution[K, T] with HasName =
    new Distribution[K, T] with HasName:
      override def generator(params: Map[Params, K]): Generator[T] = f(params)

      override def params: Set[Params] = p

      override def name: String = n

object AlgorithmFactory:
  def apply[G](f: Seq[G] => Seq[Step], n: String): Algorithm[G, Seq[State[ElementInfo[G]]]] with HasName =
    new Algorithm[G, Seq[State[ElementInfo[G]]]] with HasName:
      override def execute(s: Seq[G]): Seq[State[ElementInfo[G]]] =
        StepsTransformer().getSeqList(f(s), s).map(s => State(s))

      override def name: String = n

  object State:
    def apply[T](v: Seq[T]): State[T] = new State[T]:
      override def get: Seq[T] = v

case class IntModelImpl() extends IntModel with Algorithms with Distributions with StateResult:
  import model.Params.*
  export model.Params.*
  given Generable[Int] = x => x.toInt


  override def algorithms: Set[Algorithm[ValType, ResultType] with HasName] =
    Set(AlgorithmFactory(bubbleSort, "Bubble sort"),
      AlgorithmFactory(mergeSort, "Merge sort"),
      AlgorithmFactory(insertionSort, "Insertion sort"),
      AlgorithmFactory(quickSort, "Quicksort"),
      AlgorithmFactory(heapSort, "Heap sort"))

  override def distributions: Set[Distribution[ParamsType, ValType] with HasName] =

    Set(DistributionFactory(p =>
      given Conversion[Params, Int] = x => p(x)
      GaussianDistribution(Size / 2, Std, 1, 10000, DuplicatesPercentage),
      Set(Size, Std, DuplicatesPercentage), "Gaussian"),
      DistributionFactory(p =>
        given Conversion[Params, Int] = x => p(x)
        UniformDistribution(Min, Max, DuplicatesPercentage),
        Set(Min, Max, DuplicatesPercentage), "Uniform"))