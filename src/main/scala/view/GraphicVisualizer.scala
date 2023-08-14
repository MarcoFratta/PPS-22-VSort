package view

import com.raquo.laminar.api.L.*
import model.{ElementInfo, State}
import org.scalajs.dom
import org.scalajs.dom.html
import view.BottomBar

import java.util.{Timer, TimerTask}
import scala.annotation.tailrec



private case class RectanglesVisualizer(nRect: Int, maxValue: Int):
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
  def clear: Unit =
    val ctx = canvasElem.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    index =0
    ctx.clearRect(0, 0, canvasElem.width, canvasElem.height)



case class GraphicVisualizer(seq: Seq[State[Int]]):
  private val bottomBar = BottomBar(this)
  private val seqStep: Seq[State[Int]] = seq
  private var starterSeq = seqStep
  private val visualizer: RectanglesVisualizer = RectanglesVisualizer(seqStep.head.get.size,
    seqStep.head.get.map(a => a.value).max)
  private var index: Int = 0
  private var period: Int = 100
  private var timer = new Timer()
  private var isExecuting: Boolean = false
  showGraphic
  appendBottomBar


  private def appendBottomBar: Unit =
    if dom.document.querySelector(".bottomBar").innerHTML == "" then
      render(dom.document.querySelector(".bottomBar"), bottomBar.renderBottomBar)
    else
      dom.document.querySelector(".bottomBar").innerHTML= ""
      render(dom.document.querySelector(".bottomBar"), bottomBar.renderBottomBar)


  private def getColourFromProperties(elementInfo: ElementInfo[Int]): String =
    elementInfo match
      case _ if elementInfo.selected => "green"
      case _ if elementInfo.compared => "blue"
      case _ if elementInfo.hidden => "black"
      case _ => "red"

  private def showGraphic: Unit =
    val list1 = seqStep(index)
    visualizer.clear
    @tailrec
    def drawList(l: List[ElementInfo[Int]]): Unit =
      l match
        case h :: t =>
          visualizer.drawSingleRectangle(h.value, getColourFromProperties(h))
          drawList(t)
        case _ =>
    drawList(list1.get.toList)

  def play: Unit =
    bottomBar.changePlayIcon
    timer = new Timer()
    isExecuting = true
    val task = new TimerTask() {
      def run() = nextStep
    }
    timer.schedule(task, 0, period)

  def nextStep: Unit =
    bottomBar.enableBackButton(true)
    if index == seqStep.size - 1
    then
      bottomBar.disableNextButton(true)
      end
    else index = index + 1
    showGraphic

  def backStep: Unit =
    if index equals 1 then
      bottomBar.enableBackButton(false)
    index = index - 1
    if index < seqStep.size then
      bottomBar.disableNextButton(false)
      bottomBar.changeStopIcon
    showGraphic


  def stop: Unit =
    isExecuting = false
    timer.cancel()
    bottomBar.changeStopIcon

  def replay: Unit =
    index = 0
    starterSeq = seqStep
    bottomBar.enableBackButton(false)
    bottomBar.disableNextButton(false)
    stop
    isExecuting = false
    showGraphic

  def end: Unit =
    isExecuting = false
    timer.cancel()
    bottomBar.disableNextButton(true)

  def setSpeed(speed: Int): Unit =
    timer.cancel()
    timer = new Timer()
    println("periodo prima "+ period)
    val task = new TimerTask() {
      def run(): Unit = nextStep
    }
    period = 1001 - speed
    println("period dopo " + period)
    if isExecuting
    then timer.schedule(task, 0, period)
