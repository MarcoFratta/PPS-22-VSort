package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.*
import model.SortingAlgorithms.mergeSort
import model.Step.Swap
import model.sortModel.SortOperations.*
import view.rectangles.GraphFunctions




object StepController:
  
  given Generable[Int] = x => x.toInt

  case class RangeGaussian[T: Generable]()
    extends GaussianGen[T](75, 25)
      with Shifted[T](1, 10000)

  import model.sortModel.SortOperations.given

  
  var seq: Seq[Int] = RangeGaussian().generateAll(0 to 100).toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
  var steps: Seq[Step] = mergeSort(seq)
  private var example: Seq[Seq[ElementInfo[Int]]] = StepsVisualizer[Int].getSeqList(steps, seq)


  class SeqProp:
    def getElements: Seq[Seq[ElementInfo[Int]]] = example
    def setSize(size: Int): Unit = changeSize(size)
  def setSeqList(): Unit =
    GraphFunctions.setSeqList(new SeqProp())

  def changeSize(size: Int): Unit =
    seq = RangeGaussian().generateAll(0 until size).toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
    steps = mergeSort(seq)
    example = StepsVisualizer[Int].getSeqList(steps, seq)
    setSeqList()
    