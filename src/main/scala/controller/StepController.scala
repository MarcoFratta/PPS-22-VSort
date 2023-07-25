package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.*
import model.SortingAlgorithms.mergeSort
import model.Step.Swap
import model.sortModel.SortOperations.*
import view.{GraphFunctions, View}

object StepController:
  
  given Generable[Int] = x => x.toInt

  case class RangeGaussian[T: Generable]()
    extends GaussianGen[T](75, 25)
      with Shifted[T](1, 10000)

  import model.sortModel.SortOperations.given

  
  var seq: Seq[Int] = RangeGaussian().generateAll(0 to 100).toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
  var steps: Seq[Step] = mergeSort(seq)
  var example: Seq[Seq[ElementInfo[Int]]] = StepsTransformer[Int].getSeqList(steps, seq)


  trait SeqProp:
    def getElements: Seq[Seq[ElementInfo[Int]]]
    def setSize(size: Int): GraphFunctions
    def setSeqList(): GraphFunctions

  case class SeqProperties(view: View) extends SeqProp:
    override def getElements: Seq[Seq[ElementInfo[Int]]] =
      println("elements" )
      example
      //example
    override def setSize(size: Int): GraphFunctions  =
      seq = RangeGaussian().generateAll(0 until size).toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
      steps = mergeSort(seq)
      example = StepsTransformer[Int].getSeqList(steps, seq)
      view.setSeqList(example)
    override def setSeqList(): GraphFunctions =
      view.setSeqList(example)
