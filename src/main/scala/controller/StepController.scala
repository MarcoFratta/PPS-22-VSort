package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SortingAlgorithms.mergeSort
import model.Step.Swap
import model.seqProperties.*
import model.sortModel.SortOperations.*
import view.GraphFunctions

import scala.collection.immutable.Map

object StepController:
  trait SeqProp extends ModelTypes:
    def getElements: ResultType

  case class SeqProperties(prop: Properties with IntTypes) extends SeqProp with IntTypes:
    private def findParamFromName(name: String): Params =
      prop.params.filter(a => a._1.toString equals name).toList.head._1

    override def getElements: ResultType =
      println("Entering getElements")

      val seq = prop.distribution.generator(prop.params).generateAll(0 to
        prop.params(findParamFromName("Size"))).toList.sortWith(prop.distribution.compare).map(x => x._2)
      println("seq" + seq)
      prop.algorithm.execute(seq)

