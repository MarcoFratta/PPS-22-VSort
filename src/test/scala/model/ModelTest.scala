package model

import model.*
import model.api.Comparable
import model.component.Params.*
import model.component.{AlgorithmFactory, DistributionFactory, IntOrderings, Params}
import model.properties.*
import model.properties.Modifier.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ModelTest extends AnyFlatSpec with Matchers:

  import model.algorithms.SortingAlgorithms.{*, given}

  given Generable[Int] = x => x.toInt

  given c: Comparable[(Int, Int)] = IntOrderings.ascendingXOrder


  "an algorithm factory" must "exists" in {
    val model = AlgorithmFactory.intAlgorithm[Int](bubbleSort, "bubble sort")
  }

  "a Distribution factory" must "exists" in {


    val model = DistributionFactory(p => GaussianGen(4,5), Set.empty, "Gaussian")
  }


  case class DistributionTest[T: Generable](mean:Int, std:Int, mi:Int,ma:Int,percentage:Int)
    extends GaussianGen[T](mean, std)
      with Shifted[T](mi,ma)
      with Duplicated(percentage/100)

  "a Distribution with different modifiers" must "exists" in {


    val model = DistributionFactory(p => DistributionTest(4,5,6,7,100), Set.empty, "Gaussian")
  }

  "a distribution created with factory" must "give correct values" in {
    val model =  DistributionFactory[Int,Int](p =>
      given Conversion[Params, Int] = x => p(x)
      DistributionTest(50,Std, Min, Max,DuplicatesPercentage),
      Set(Std, Min, Max,DuplicatesPercentage), "Gaussian")

    val g = model.generator(Map((Std,15),(Min,1),(Max, 60),(DuplicatesPercentage, 100)))
    g.generateAll(5 to 100 by 2).forall((x,y) => y >= 1 && y <= 60) mustBe true
  }