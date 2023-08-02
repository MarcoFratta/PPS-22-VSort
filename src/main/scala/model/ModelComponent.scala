package model

import controller.*
import model.*
import model.SortingAlgorithms.*
import model.seqProperties.*
import model.seqProperties.Distributions.*
import view.*


object ModelComponent:

  object Model:
    trait Model extends ModelTypes with Algorithms with Distributions

    trait Provider:
      val model: Model with IntTypes

    trait Component:
      case class ModelImpl() extends Model with IntTypes:

        import model.Params.*
        import model.SortingAlgorithms.{*, given}

        given Generable[Int] = x => x.toInt

        override def algorithms: Set[Algorithm[ValType, ResultType] with HasName] =
          Set(AlgorithmFactory.intAlgorithm(bubbleSort, "Bubble sort"),
            AlgorithmFactory.intAlgorithm(mergeSort, "Merge sort"),
            AlgorithmFactory.intAlgorithm(insertionSort, "Insertion sort"),
            AlgorithmFactory.intAlgorithm(quickSort, "Quicksort"),
            AlgorithmFactory.intAlgorithm(heapSort, "Heap sort"))

        override def distributions: Set[Distribution[ParamsType, ValType] with HasName] =

          Set(DistributionFactory(p =>
            given Conversion[Params, Int] = x => p(x)

            GaussianDistribution(Size / 2, Std, 1, 1000),
            Set(Size, Std), "Gaussian"),
            DistributionFactory(p =>
              given Conversion[Params, Int] = x => p(x)

              UniformDistribution(Min, Max, DuplicatesPercentage, Size),
              Set(Min, Max, DuplicatesPercentage, Size), "Uniform"))


    trait Interface extends Provider with Component