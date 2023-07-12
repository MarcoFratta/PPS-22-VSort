package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.Generators.uniformDistribution
import model.SortingAlgorithms.bubbleSort
import model.Step.Swap
import model.sortModel.SortOperations.*

case class UniformDistribution(min: Int, max:Int, size:Int)

object Graphic:

  import model.sortModel.SortOperations.given
  import view.rectangles.*


  var seq = uniformDistribution(0, 100).take(50).map(a => a.toInt)
  var steps: Seq[Step] = bubbleSort(seq)
  var index: Int = 0

  def showGraph(): Unit =
    drawGraphic(seq.toList, Var(seq.size))

  import java.util.{Timer, TimerTask}

  def play(): Unit =
    println("play")
    val timer = new Timer()
    val task = new TimerTask {
      def run(): Unit = {
        nextStep()
      }
    }
    timer.schedule(task, 0, 10)

  def nextStep(): Unit =
    println("next step")
    steps(index) match
    case Swap(a: Int, b: Int) =>
      println("a" + a + "b" + b)
      seq = seq.updated(a, seq(b)).updated(b, seq(a))
      index = index + 1
      drawGraphic(seq.toList, Var(seq.size))
    case Step.Selection(a: Int, b:Int) =>
      println("selection")
      colorRect(List(a,b), "blue")
      index = index + 1
    case Step.Comparison(a: Int, b: Int) =>
      println("comp")
      colorRect(List(a,b), "yellow")
      index = index + 1
    case _ => println("altro")

