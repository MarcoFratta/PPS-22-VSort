package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.*
import model.Step.Swap
import model.sortModel.SortOperations.*
import model.SortingAlgorithms.*

import java.util.concurrent.ScheduledFuture
import java.util.{Timer, TimerTask}


object Graphic:

  given Generable[Int] = x => x.toInt

  case class RangeGaussian[T: Generable]()
    extends GaussianGen[T](75, 25)
      with Shifted[T](1, 10000)

  import model.sortModel.SortOperations.given
  import view.rectangles.*
  import view.livechart.BottomBar.*
  import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
  import model.StepsVisualizer.*



  var seq = RangeGaussian().generateAll(0 to 175).toList.sortWith((a,b) => a._1 <= b._1).map(x => x._2)

  var starterSeq = seq
  var steps: Seq[Step] = mergeSort(seq)
  var example = StepsVisualizer.getSeqList(steps, seq)
  var isExecuting: Boolean = false
  var index: Int = 0
  var period: Int = 20
 // val executor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  var timer = new Timer()
  val visualizer: RectanglesVisualizer = RectanglesVisualizer(example(1).size, example(1).map(a => a.value).max)

  def getColourFromProperties(elementInfo: ElementInfo[Int]): String =
    elementInfo match
      case _ if elementInfo.selected => "green"
      case _ if elementInfo.compared => "blue"
      case _ if elementInfo.hidden => "black"
      case _ => "red"
  def showGraphSeparatedRect(): Unit =
    val list1 = example(index)
    //val visualizer: RectanglesVisualizer = RectanglesVisualizer(list1.size, list1.map(a => a.value).max)
    visualizer.clear()
    def drawList(l: List[ElementInfo[Int]]): Unit =
      l match
        case h::t =>
          visualizer.drawSingleRectangle(h.value, getColourFromProperties(h))
          drawList(t)
        case _ =>
    drawList(list1.toList)

  def showGraph(): Unit =
    drawGraphic(seq.toList, Var(seq.size))


  import java.util.{Timer, TimerTask}

  def changeSize(size: Int): Unit =
    starterSeq = RangeGaussian().generateAll(0 to 175).toList.sortWith((a,b) => a._1 <= b._1).map(x => x._2)
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
    //steps = bubbleSort(starterSeq)
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
    index = index + 1
    showGraphSeparatedRect()
    

  def backStep(): Unit =
    println("back step")
    if index equals 1 then
      enableBackButton(false)

    index = index - 1
    if index < steps.size then
      disableNextButton(false)
    showGraphSeparatedRect()


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