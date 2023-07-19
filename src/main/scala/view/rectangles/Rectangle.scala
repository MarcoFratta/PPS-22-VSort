package view.rectangles

import com.raquo.laminar.api.L.*
import controller.StepController.SeqProp
import model.ElementInfo
import org.scalajs.dom
import org.scalajs.dom.html
import view.livechart.BottomBar.{changePlayIcon, changeStopIcon, disableNextButton, enableBackButton}
import view.rectangles.GraphFunctions.showGraphic

import java.util.{Timer, TimerTask}
import scala.annotation.tailrec



case class RectanglesVisualizer(nRect: Int, maxValue: Int):
  private val canvasElem: html.Canvas = dom.document.querySelector(".canvas").asInstanceOf[dom.html.Canvas]
  private val rectangleWidth = canvasElem.width / (1.5 * nRect)
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
  private var seqStep: Seq[Seq[ElementInfo[Int]]] = Seq()
  private var starterSeq = seqStep
  private var visualizer: RectanglesVisualizer = RectanglesVisualizer(0,0)
  private var index: Int = 0
  private var period: Int = 20
  private var timer = new Timer()
  private var isExecuting: Boolean = false
  private var seqProp: Option[SeqProp] = None

  def setSeqList(seqProperties: SeqProp): Unit =
    seqStep = seqProperties.getElements
    seqProp = Some(seqProperties)
    visualizer =  RectanglesVisualizer(seqStep(1).size, seqStep(1).map(a => a.value).max)
    showGraphic()
    //replay()

  private def getColourFromProperties(elementInfo: ElementInfo[Int]): String =
    elementInfo match
      case _ if elementInfo.selected => "green"
      case _ if elementInfo.compared => "blue"
      case _ if elementInfo.hidden => "black"
      case _ => "red"

  private def showGraphic(): Unit =
    val list1 = seqStep(index)
    visualizer.clear()
    @tailrec
    def drawList(l: List[ElementInfo[Int]]): Unit =
      l match
        case h :: t =>
          visualizer.drawSingleRectangle(h.value, getColourFromProperties(h))
          drawList(t)
        case _ =>
    drawList(list1.toList)

  def play(): Unit =
    changePlayIcon()
    timer = new Timer()
    isExecuting = true
    val task = new TimerTask() {
      def run(): Unit = index match
        case _ if index equals seqStep.size-1 => end()
        case _ => nextStep()
    }
    timer.schedule(task, 0, period)

  def nextStep(): Unit =
    enableBackButton(true)
    if index == seqStep.size - 1
    then
      disableNextButton(true)
      end()
    else index = index + 1
    showGraphic()

  def backStep(): Unit =
    if index equals 1 then
      enableBackButton(false)
    index = index - 1
    if index < seqStep.size then
      disableNextButton(false)
      changeStopIcon()
    showGraphic()


  def stop(): Unit =
    isExecuting = false
    timer.cancel()
    changeStopIcon()

  def replay(): Unit =
    index = 0
    starterSeq = seqStep
    enableBackButton(false)
    disableNextButton(false)
    stop()
    isExecuting = false
    showGraphic()

  def end(): Unit =
    isExecuting = false
    timer.cancel()
    disableNextButton(true)

  def setSpeed(speed: Int): Unit =
    timer.cancel()
    timer = new Timer()
    val task = new TimerTask() {
      def run(): Unit = index match
        case _ if index equals seqStep.size-1 => end()
        case _ => nextStep()
    }
    period = speed
    if isExecuting
    then timer.schedule(task, 0, speed)

  def changeSize(size: Int): Unit =
    seqProp.get.setSize(size)
    replay()