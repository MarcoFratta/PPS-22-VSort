package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.seqProperties.*
import model.SortingAlgorithms.mergeSort
import model.Step.Swap
import model.sortModel.SortOperations.*
import view.{GraphFunctions, View}

import scala.collection.immutable.Map

object StepController:
  trait SeqProp[T]:
    def getElements: Seq[State[ElementInfo[T]]]

  case class SeqProperties(prop: Properties) extends SeqProp[Int]:
    private def findParamFromName(name: String): Params =
      prop.paramMap.filter(a => a._1.toString equals name).toList.head._1
    override def getElements: Seq[State[ElementInfo[Int]]] =
      val seq: Seq[Int] = prop.distribution.generator(prop.paramMap).generateAll(0 to 
        prop.paramMap(findParamFromName("Size"))).toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
      prop.alg.execute(seq)

