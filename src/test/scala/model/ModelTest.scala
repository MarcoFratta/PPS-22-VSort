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


    val model = DistributionFactory(p => DistributionTest(4,5,6,7,100), Set.empty, "Gaussian")
  }

  "a distribution created with factory" must "give correct values" in {
    val model =  DistributionFactory[Int,Int](p =>
      DistributionTest(p("mean"),p("std"),p("min"), p("max"),p("percentage")),
      Set("mean","std","min","max","percentage"), "Gaussian")

    val g = model.generator(Map(("mean", 200),("std",15),("min",1),("max", 60),("percentage", 100)))
    g.generateAll(5 to 100 by 2).forall((x,y) => y >= 1 && y <= 60) mustBe true
  }