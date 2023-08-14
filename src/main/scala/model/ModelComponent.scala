package model

import controller.*
import model.*
import model.SortingAlgorithms.*
import model.seqProperties.*
import model.seqProperties.Distributions.*
import model.sortModel.Comparable
import view.*
import controller.Properties

import javax.print.attribute.HashPrintRequestAttributeSet


object ModelComponent:


  object Model:
    trait Model extends ModelTypes with Algorithms with Distributions:
      def getData(p: Properties with IntTypes): ResultType

    trait Provider:
      val model: Model with IntTypes

    trait Component:
      case class ModelImpl() extends Model with IntTypes:

        import model.Params.*
        import model.SortingAlgorithms.{*, given}



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
          import model.IntOrderings.*
          given Generable[Int] = x => x.toInt + 1
          Set(DistributionFactory(p =>
            given Conversion[Params, Int] = x => p(x)

            GaussianDistribution(Size / 2, Std, 1, 10000),
            Set(Size, Std), "Gaussian", IntOrderings.ascendingXOrder),
            DistributionFactory(p =>
              given Conversion[Params, Int] = x => p(x)

              UniformDistribution(1, Size + 1, DuplicatesPercentage),
              Set(DuplicatesPercentage, Size), "Uniform", IntOrderings.randomOrder),
            DistributionFactory(p =>
              given Conversion[Params, Int] = x => p(x)

              UniformDistribution(1, Size + 1, DuplicatesPercentage),
              Set(DuplicatesPercentage, Size), "Ascending order", IntOrderings.ascendingYOrder),
            DistributionFactory(p =>
              given Conversion[Params, Int] = x => p(x)

              UniformDistribution(1, Size + 1, DuplicatesPercentage),
              Set(DuplicatesPercentage, Size), "Descending order", IntOrderings.descendingYOrder))
    trait Interface extends Provider with Component