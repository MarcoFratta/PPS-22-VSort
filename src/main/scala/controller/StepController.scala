package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.Generators.uniformDistribution
import model.SortingAlgorithms.bubbleSort
import model.Step.Swap
import model.sortModel.SortOperations.*

import java.util.concurrent.ScheduledFuture
import java.util.{Timer, TimerTask}

case class UniformDistribution(min: Int, max:Int, size:Int)

object Graphic:

  import model.sortModel.SortOperations.given
  import view.rectangles.*
  import view.livechart.BottomBar.*
  import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}


  var seq = uniformDistribution(0, 100).take(100).map(a => a.toInt)
  var example = seq.toList
  var starterSeq = seq
  var steps: Seq[Step] = bubbleSort(seq)
  var isExecuting: Boolean = false
  var index: Int = 0
  var period: Int = 20
 // val executor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  var timer = new Timer()
  def showGraphSeparatedRect(): Unit =
    val visualizer: RectanglesVisualizer = RectanglesVisualizer(example.size, example.max)

    def drawList(l: List[Int]): Unit =
      l match
        case h::t =>
          visualizer.drawSingleRectangle(h, if h < 10 then "red" else "purple")
          drawList(t)
        case _ =>
    drawList(example)

  def showGraph(): Unit =
    drawGraphic(seq.toList, Var(seq.size))


  import java.util.{Timer, TimerTask}

  def changeSize(size: Int): Unit =
    starterSeq = uniformDistribution(0, 100).take(size).map(a => a.toInt)
    replay()


  def replay(): Unit =
    index = 0
    seq = starterSeq
    
    enableBackButton(false)
    disableNextButton(false)
    stop()
    isExecuting = false
    showGraph()

  def play(): Unit =
    println("play")
    changePlayIcon()
    steps = bubbleSort(starterSeq)
    timer = new Timer()
    isExecuting = true
    val task = new TimerTask() {
      def run(): Unit = index match
        case _ if index equals steps.size  => end()
        case _ => nextStep()

    }
    timer.schedule(task, 0, period)
    //executor.scheduleAtFixedRate(task, 0, 20, TimeUnit.MILLISECONDS)

  def end(): Unit =
    println("size: "+ steps.size)
    println("index "+ index)
    println("ultimo step"+ steps.last.toString)
    println("fine")
    isExecuting = false
    timer.cancel()
    disableNextButton(true)
  def nextStep(): Unit =
    //println("next step")
    enableBackButton(true)
    println("step size " + steps.size)
    println("index " + index)
    if index >= steps.size - 1
      then
        disableNextButton(true)
        println("index: "+  index)
        println("steps: "+ steps.size)
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

    index = index - 1
    if index < steps.size then
      disableNextButton(false)
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
    isExecuting = false
    timer.cancel()
    changeStopIcon()


  def setSpeed(speed: Int): Unit =
    println("setting speed")
    timer.cancel()
    timer = new Timer()
    val task = new TimerTask() {
      def run(): Unit = index match
        case _ if index equals steps.size => end()
        case _ => nextStep()

    }
    period = speed
    if isExecuting
      then timer.schedule(task, 0, speed)
    //executor.shutdownNow()
    //executor.schedule(task, speed, TimeUnit.MILLISECONDS)