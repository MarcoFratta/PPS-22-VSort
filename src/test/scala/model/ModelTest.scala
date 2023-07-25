package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import model.*
import model.seqProperties.{GaussianGen, Generable}
import model.seqProperties.*
import model.seqProperties.Modifier.*

class ModelTest extends AnyFlatSpec with Matchers:

  import SortingAlgorithms.{*,given}
  given Generable[Int] = x => x.toInt


  "an algorithm factory" must "exists" in {
    val model = AlgorithmFactory[Int](bubbleSort, "bubble sort")
  }

  "a Distribution factory" must "exists" in {


    val model = DistributionFactory(p => GaussianGen(4,5), Set.empty, "Gaussian")
  }


  case class DistributionTest[T: Generable](mean:Int, std:Int, mi:Int,ma:Int,percentage:Int)
    extends GaussianGen[T](mean, std)
      with Shifted[T](mi,ma)
      with Duplicated(percentage/100)

  "a Distribution with different modifiers" must "exists" in {


    val model = DistributionFactory(p => DistributionTest(4,5,6,7,8), Set.empty, "Gaussian")
  }

//  "an algorithm factory" must "exists" in {
//    val model = AlgorithmFactory()
//  }