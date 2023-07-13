package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.Generators.uniformDistribution
import model.SortingAlgorithms.bubbleSort
import model.Step.Swap
import model.sortModel.SortOperations.*

import java.util.Timer

case class UniformDistribution(min: Int, max:Int, size:Int)

object Graphic:

  import model.sortModel.SortOperations.given
  import view.rectangles.*
  import view.livechart.BottomBar.*


  var seq = uniformDistribution(0, 100).take(20).map(a => a.toInt)
  var example = seq.toList
  val starterSeq = seq
  var steps: Seq[Step] = bubbleSort(seq)

  var index: Int = 0

  var timer = new Timer()
  def showGraph(): Unit =
    val visualizer: RectanglesVisualizer = RectanglesVisualizer(example.size, example.max)

    def drawList(l: List[Int]): Unit =
      l match
        case h::t =>
          visualizer.drawSingleRectangle(h, if h < 10 then "red" else "purple")
          drawList(t)
        case _ =>
    drawList(example)

    //drawGraphic(seq.toList, Var(seq.size))


  import java.util.{Timer, TimerTask}

  def replay(): Unit =
    index = 0
    seq = starterSeq
    enableBackButton(false)
    disableNextButton(false)
    stop()
    showGraph()

  def play(): Unit =
    println("play")
    changePlayIcon()

    val task = new TimerTask {
      def run(): Unit = index match
        case _ if index equals (steps.size -1) => end()
        case _ => nextStep()

    }
    timer = new Timer()
    timer.schedule(task, 0, 20)

  def end(): Unit =
    println("fine")
    timer.cancel()
    disableNextButton(true)
  def nextStep(): Unit =
    //println("next step")
    enableBackButton(true)
    if index < steps.size -1
      then disableNextButton(false)
    if index equals steps.size
      then end()
    steps(index) match
    case Swap(a: Int, b: Int) =>
      //println("a" + a + "b" + b)
      seq = seq.updated(a, seq(b)).updated(b, seq(a))
      index = index + 1
      drawGraphic(seq.toList, Var(seq.size))
    case Step.Selection(a: Int, b:Int) =>
      //println("selection")
      colorRect(List(a,b), "blue")
      index = index + 1
    case Step.Comparison(a: Int, b: Int) =>
     // println("comp")
      drawGraphic(seq.toList, Var(seq.size))
      colorRect(List(a,b), "yellow")
      index = index + 1
    case _ => println("altro")

  def backStep(): Unit =
    println("back step")
    if index equals 1 then
      enableBackButton(false)

    if index < steps.size-1 then
      disableNextButton(false)
    index = index - 1
    steps(index) match
    case Swap(a: Int, b: Int) =>
        println("a" + a + "b" + b)
        seq = seq.updated(b, seq(a)).updated(a, seq(b))
        drawGraphic(seq.toList, Var(seq.size))
    case Step.Selection(a: Int, b: Int) =>
        println("selection")
        colorRect(List(a, b), "blue")

    case Step.Comparison(a: Int, b: Int) =>
        println("comp")
        colorRect(List(a, b), "yellow")

    case _ => println("altro")


  def stop(): Unit =
    println("stop")
    timer.cancel()
    changeStopIcon()


