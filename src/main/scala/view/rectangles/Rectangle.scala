package view.rectangles

import com.raquo.laminar.api.L.*
import controller.StepController.SeqProp
import model.ElementInfo
import org.scalajs.dom
import org.scalajs.dom.html
import view.livechart.BottomBar.{changePlayIcon, changeStopIcon, disableNextButton, disableReplayButton, enableBackButton}
import view.rectangles.GraphFunctions.showGraphSeparatedRect

import java.util.{Timer, TimerTask}



case class RectanglesVisualizer(nRect: Int, maxValue: Int):
  val canvasElem: html.Canvas = dom.document.querySelector(".canvas").asInstanceOf[dom.html.Canvas]
  val rectangleWidth = canvasElem.width / (1.5 * nRect)
  var index = 0
  def drawSingleRectangle(value: Int, color: String): Unit =
    val ctx = canvasElem.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val x = index * (rectangleWidth + 0.5)
    ctx.fillStyle = color
    val height = (value * canvasElem.height) / maxValue
    index = index + 1
    ctx.fillRect(x, canvasElem.height - height, rectangleWidth, height)
  def clear(): Unit =
    val ctx = canvasElem.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    index =0
    ctx.clearRect(0, 0, canvasElem.width, canvasElem.height) 


object GraphFunctions:
  var seqStep:Seq[Seq[ElementInfo[Int]]] = Seq()
  var starterSeq = seqStep
  var visualizer: RectanglesVisualizer = RectanglesVisualizer(0,0)
  var index: Int = 0
  var period: Int = 20
  // val executor: ScheduledExecutorService = Executors.newScheduledThreadPool(1)
  var timer = new Timer()
  var isExecuting: Boolean = false
  var seqProp: SeqProp = null

  def setSeqList(seqProperties: SeqProp): Unit =
    seqStep = seqProperties.getElements()
    seqProp = seqProperties
    visualizer =  RectanglesVisualizer(seqStep(1).size, seqStep(1).map(a => a.value).max)
    showGraphSeparatedRect()
    //replay()

  private def getColourFromProperties(elementInfo: ElementInfo[Int]): String =
    elementInfo match
      case _ if elementInfo.selected => "green"
      case _ if elementInfo.compared => "blue"
      case _ if elementInfo.hidden => "black"
      case _ => "red"

  private def showGraphSeparatedRect(): Unit =
    val list1 = seqStep(index)
    visualizer.clear()
    def drawList(l: List[ElementInfo[Int]]): Unit =
      l match
        case h :: t =>
          visualizer.drawSingleRectangle(h.value, getColourFromProperties(h))
          drawList(t)
        case _ =>
    drawList(list1.toList)

  def play(): Unit =
    println("play")
    changePlayIcon()
    //steps = bubbleSort(starterSeq)
    timer = new Timer()
    isExecuting = true
    val task = new TimerTask() {
      def run(): Unit = index match
        case _ if index equals seqStep.size-1 => end()
        case _ => nextStep()

    }
    timer.schedule(task, 0, period)

  def nextStep(): Unit =
    //println("next step")
    enableBackButton(true)
    if index == seqStep.size - 1
    then
      disableNextButton(true)
      end()
      println("index: " + index)
      println("steps: " + seqStep.size)
    //if index equals seqStep.size then end()
    else index = index + 1
    showGraphSeparatedRect()

  def backStep(): Unit =
    println("back step")
    if index equals 1 then
      enableBackButton(false)
    index = index - 1
    if index < seqStep.size then
      disableNextButton(false)

      changeStopIcon()
    showGraphSeparatedRect()


  def stop(): Unit =
    println("stop")
    isExecuting = false
    timer.cancel()
    
    changeStopIcon()

  def replay(): Unit =
    index = 0
    starterSeq = seqStep

    enableBackButton(false)
    disableNextButton(false)
    println("replay")
    stop()
    isExecuting = false
    showGraphSeparatedRect()

  def end(): Unit =
    isExecuting = false
    timer.cancel()
    println("size: " + seqStep.size)
    println("index " + index)
    println("ultimo step" + seqStep.last.toString)
    println("fine")
    disableNextButton(true)

  def setSpeed(speed: Int): Unit =
    println("setting speed")
    timer.cancel()
    timer = new Timer()
    val task = new TimerTask() {
      def run(): Unit = index match
        case _ if index equals seqStep.size => end()
        case _ => nextStep()

    }
    period = speed
    if isExecuting
    then timer.schedule(task, 0, speed)

  def changeSize(size: Int): Unit =
    seqProp.setSize(size)
    //seqStep = RangeGaussian().generateAll(0 to 175).toList.sortWith((a, b) => a._1 <= b._1).map(x => x._2)
    replay()