package model

import model.seqProperties.Generator
import model.StepsTransformer
import model.InputType
import model.seqProperties.Generable
import model.SortingAlgorithms.{*, given}
import model.sortModel.Distributions.{GaussianDistribution, UniformDistribution}

trait HasName:
  def name:String
trait State[T]:
  def get:Seq[T]
trait Distribution[K, T: Generable]:
  def generator(params:Map[String, K]):Generator[T]
  def params:Set[String]
trait Algorithm[G,R]:
  def execute(s:Seq[G]):R

trait Model:

  type ParamsType
  type ResultType
  type ArgType

trait Algorithms extends Model:
  def algorithms: Set[Algorithm[ArgType, ResultType] with HasName]
trait Distributions extends Model:
  def distributions: Set[Distribution[ParamsType, ArgType] with HasName]


object DistributionFactory:
  def apply[K,T:Generable](f:Map[String,K] => Generator[T], p:Set[String], n:String): Distribution[K, T] with HasName =
    new Distribution[K,T] with HasName:
      override def generator(params: Map[String, K]): Generator[T] = f(params)
      override def params: Set[String] = p
      override def name:String = n

object AlgorithmFactory:
  object State:
    def apply[T](v:Seq[T]):State[T] = new State[T]:
      override def get: Seq[T] = v

  def apply[G](f:Seq[G] => Seq[Step], n:String):Algorithm[G,Seq[State[ElementInfo[G]]]] with HasName =
    new Algorithm[G,Seq[State[ElementInfo[G]]]] with HasName:
      override def execute(s: Seq[G]): Seq[State[ElementInfo[G]]] =
        StepsTransformer().getSeqList(f(s),s).map(s => State(s))

      override def name: String = n

trait IntModel extends Model:
  override type ParamsType = Int
  override type ArgType = Int

trait StateResult extends Model:
  override type ResultType = Seq[State[ElementInfo[ArgType]]]
case class ModelImpl() extends IntModel with Algorithms with Distributions with StateResult:
  import model.sortModel.SortOperations.comp

  override def algorithms: Set[Algorithm[Int, Seq[State[ElementInfo[Int]]]] with HasName] =
    Set(AlgorithmFactory(bubbleSort,"Bubble sort"),
      AlgorithmFactory(mergeSort,"Merge sort"),
      AlgorithmFactory(insertionSort,"Insertion sort"),
      AlgorithmFactory(quickSort,"Quicksort"),
      AlgorithmFactory(heapSort,"Heap sort"))

  override def distributions: Set[Distribution[ParamsType,ArgType] with HasName] =
    given Generable[Int] = x => x.toInt
    Set(DistributionFactory(p => GaussianDistribution(p("size") / 2, p("std"), 1,10000, p("percentage")),
      Set("size","std","percentage"), "Gaussian"),
    DistributionFactory(p => UniformDistribution(p("min"),p("max"),p("percentage")),
    Set("min","max","percentage"), "Uniform"))