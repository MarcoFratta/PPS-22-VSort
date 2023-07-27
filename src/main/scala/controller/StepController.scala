package controller
import com.raquo.laminar.api.L.Var
import controller.StepController.{seq, steps}
import model.*
import model.seqProperties.*
import model.SortingAlgorithms.mergeSort
import model.Step.Swap
import model.sortModel.SortOperations.*
import view.{GraphFunctions, View}

import scala.collection.immutable.Map

object StepController:
  
  given Generable[Int] = x => x.toInt

  case class RangeGaussian[T: Generable](mi: Int, ma: Int)
    extends GaussianGen[T](75, 25)
      with Shifted[T](mi, ma)

  import model.sortModel.SortOperations.given

  
  var seq: Seq[Int] = RangeGaussian(5,10).generateAll(0 to 100).toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
  var steps: Seq[Step] = mergeSort(seq)
  var example: Seq[Seq[ElementInfo[Int]]] = StepsTransformer[Int].getSeqList(steps, seq)


  trait SeqProp:
    def getElements: Seq[Seq[ElementInfo[Int]]]

  case class SeqProperties(prop: Properties) extends SeqProp:
    private def findParamFromName(name: String): Params =
      prop.map.filter(a => a._1.toString equals name).toList.head._1
    override def getElements: Seq[Seq[ElementInfo[Int]]] =
      println("ottenuti elementi" )
      println(prop)
      //0 to prop.map(findParamFromName("Size"))
      var seq: Seq[Int] = prop.distribution.generator(prop.map).generateAll(0 to 50).
        toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
      //var steps: Seq[Step] = prop.alg.execute(seq).map(a => a.get)
      var example: Seq[Seq[ElementInfo[Int]]] = prop.alg.execute(seq).map(a => a.get)

      example
      //example

      //view.setSeqList(example)

