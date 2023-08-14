package view

import com.raquo.laminar.api.L.*
import model.{ElementInfo, State}
import org.scalajs.dom
import org.scalajs.dom.html
import view.BottomBar

import java.util.{Timer, TimerTask}
import scala.annotation.tailrec


object RectanglesVisualizer:
  private var canvasElem: html.Canvas = dom.document.querySelector(".canvas").asInstanceOf[dom.html.Canvas]
  var index = 0
  private var maxValue = 0
  private var rectangleWidth = 0.0
  def setDimension(nRect: Int, mValue: Int): Unit =
    clear
    maxValue = mValue
    rectangleWidth = canvasElem.width / (1.5 * nRect)


  def drawSingleRectangle(value: Int, color: String): Unit =

    val ctx = canvasElem.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    val x = index * (rectangleWidth + 0.5)
    ctx.fillStyle = color
    val height = (value * canvasElem.height) / maxValue
    index = index + 1
    ctx.fillRect(x, canvasElem.height - height, rectangleWidth, height)
  def clear: Unit =
    canvasElem = dom.document.querySelector(".canvas").asInstanceOf[dom.html.Canvas]
    val ctx = canvasElem.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    index =0
    ctx.clearRect(0, 0, canvasElem.width, canvasElem.height)



object GraphicVisualizer:
  private var seqStep: Seq[State[Int]] = Seq()
  private var starterSeq = seqStep
  private var index: Int = 0
  private var period: Int = 100
  private var timer = new Timer()
  private var isExecuting: Boolean = false

  def getData(seq: Seq[State[Int]]): Unit =
    seqStep = seq
    starterSeq = seq
    RectanglesVisualizer.setDimension(seqStep.head.get.size,
      seqStep.head.get.map(a => a.value).max)
    showGraphic
    replay




  private def getColourFromProperties(elementInfo: ElementInfo[Int]): String =
    elementInfo match
      case _ if elementInfo.selected => "green"
      case _ if elementInfo.compared => "blue"
      case _ if elementInfo.hidden => "black"
      case _ => "red"

  private def showGraphic: Unit =
    val list1 = seqStep(index)
    RectanglesVisualizer.clear
    @tailrec
    def drawList(l: List[ElementInfo[Int]]): Unit =
      l match
        case h :: t =>
          RectanglesVisualizer.drawSingleRectangle(h.value, getColourFromProperties(h))
          drawList(t)
        case _ =>
    drawList(list1.get.toList)

  def play: Unit =
    BottomBar.changePlayIcon
    timer = new Timer()
    isExecuting = true
    val task = new TimerTask() {
      def run() = nextStep
    }
    timer.schedule(task, 0, period)

  def nextStep: Unit =
    BottomBar.enableBackButton(true)
    if index == seqStep.size - 1
    then
      BottomBar.disableNextButton(true)
      end
    else index = index + 1
    showGraphic

  def backStep: Unit =
    if index equals 1 then
      BottomBar.enableBackButton(false)
    index = index - 1
    if index < seqStep.size then
      BottomBar.disableNextButton(false)
      BottomBar.changeStopIcon
    showGraphic


  def stop: Unit =
    isExecuting = false
    timer.cancel()
    BottomBar.changeStopIcon

  def replay: Unit =
    index = 0
    starterSeq = seqStep
    BottomBar.enableBackButton(false)
    BottomBar.disableNextButton(false)
    stop
    isExecuting = false
    showGraphic

  def end: Unit =
    isExecuting = false
    timer.cancel()
    BottomBar.disableNextButton(true)

  def setSpeed(speed: Int): Unit =
    timer.cancel()
    timer = new Timer()
    val task = new TimerTask() {
      def run(): Unit = nextStep
    }
    period = 1001 - speed
    if isExecuting
    then timer.schedule(task, 0, period)
