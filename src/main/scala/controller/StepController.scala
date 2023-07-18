package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.*
import model.Step.Swap
import model.sortModel.SortOperations.*
import model.SortingAlgorithms.*
import view.rectangles.GraphFunctions




object StepController:
  
  given Generable[Int] = x => x.toInt

  case class RangeGaussian[T: Generable]()
    extends GaussianGen[T](75, 25)
      with Shifted[T](1, 10000)

  import model.sortModel.SortOperations.given

  
  var seq = RangeGaussian().generateAll(0 to 100).toList.sortWith((a,b) => a._1 <= b._1).map(x => x._2)
  var steps: Seq[Step] = mergeSort(seq)
  var example: Seq[Seq[ElementInfo[Int]]] = StepsVisualizer.getSeqList(steps, seq)

  def setSeqList(): Unit =
    GraphFunctions.setSeqList(example)

  def changeSize(size: Int) =
    seq = RangeGaussian().generateAll(0 to size).toList.sortWith((a,b) => a._1 <= b._1).map(x => x._2)
    steps = mergeSort(seq)
    example = StepsVisualizer.getSeqList(steps, seq)
    setSeqList()
    