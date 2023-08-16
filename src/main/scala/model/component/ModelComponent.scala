package model.component

import controller.*
import controller.component.Properties
import model.*
import model.algorithms.SortingAlgorithms.*
import model.api.Comparable
import model.properties.*
import model.properties.Distributions.*
import view.*


object ModelComponent:

  trait Model[T] extends ModelTypes with Algorithms with Distributions:
    def getData(p: T): ResultType

  trait Provider:
    val model: Model[Properties with IntTypes] with IntTypes

  trait Observer:
    val viewModel: Algorithms with Distributions with IntTypes

  trait Component:
    case class ModelImpl() extends Model[Properties with IntTypes] with IntTypes:

      import Params.*
      import model.algorithms.SortingAlgorithms.{*, given}

      override def algorithms: Set[Algorithm[ValType, ResultType] with HasName] =
        Set(AlgorithmFactory.intAlgorithm(bubbleSort, "Bubble sort"),
          AlgorithmFactory.intAlgorithm(mergeSort, "Merge sort"),
          AlgorithmFactory.intAlgorithm(insertionSort, "Insertion sort"),
          AlgorithmFactory.intAlgorithm(quickSort, "Quicksort"),
          AlgorithmFactory.intAlgorithm(heapSort, "Heap sort"),
            AlgorithmFactory.intAlgorithm(selectionSort, "Selection sort"))

      override def getData(p: Properties with IntTypes): ResultType =
          val seq = p.distribution.generator(p.params).generateAll(0 to
            p.params(Size)).toList.sortWith(p.distribution.compare).map(x => x._2)
          p.algorithm.execute(seq)
      override def distributions: Set[Distribution[ParamsType, ValType] with HasName] =
          import DistributionFactory.*
          import Distributions.intParams
          import IntOrderings.*
          given Generable[Int] = x => x.toInt + 1

          Set(DistributionFactory((p, c) =>
            given c.type = c

            GaussianDistribution(Size / 2, Std, 1, 10000),
            Set(Size, Std), "Gaussian", IntOrderings.ascendingXOrder),
            DistributionFactory((p, c) =>
              given c.type = c

              UniformDistribution(1, Size + 1, DuplicatesPercentage),
              Set(DuplicatesPercentage, Size), "Uniform", IntOrderings.randomOrder),
            DistributionFactory((p, c) =>
              given c.type = c

              UniformDistribution(1, Size + 1, DuplicatesPercentage),
              Set(DuplicatesPercentage, Size), "Ascending order", IntOrderings.ascendingYOrder),
            DistributionFactory((p, c) =>
              given c.type = c

              UniformDistribution(1, Size + 1, DuplicatesPercentage),
              Set(DuplicatesPercentage, Size), "Descending order", IntOrderings.descendingYOrder))

  trait Interface extends Provider with Component with Observer