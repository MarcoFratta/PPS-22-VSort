package view.livechart

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import controller.SeqPropertiesController
import model.StepsVisualizer.*
import model.SortingAlgorithms.*
import org.scalajs.dom
import org.scalajs.dom.html.Canvas

import scala.scalajs.js
import scala.scalajs.js.annotation.*

@main
def LiveChart(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )
end LiveChart

trait ConversionToDouble[T]:
  def apply(x: T): Double

given ConversionToDouble[String] with
  def apply(str: String): Double = str.toDouble

given ConversionToDouble[Int] with
  def apply(int: Int): Double = int.toDouble

given ConversionToDouble[Double] with
  def apply(int: Double): Double = int


object Main:
  val minValue = 50
  val sliderValue = Var(minValue)
  import BottomBar.*
  import model.*
  import view.rectangles.*


  val seqPropertiesController = new SeqPropertiesController()
  def appElement(): Element =
    /*val data = uniformDistribution(0,100).take(50).map(a => a.toInt)
    val steps = bubbleSort(data)
    val list = getMapList(steps, data)
    val allSeq = list.map(a => a.toSeq)
    val seq = uniformDistribution(0,100).take(50).map(a => a.toInt)
    seq.foreach(println(_))
    */
    //val seq = exponentialDistribution(50, 200).take(200)
    //seq.foreach(println(_))
    //val seq = Seq(1,2,4)
    div(
        renderTopBar(sliderValue),
        // renderDataTable(),
        //renderDataList(),
        //getRectangle(seq.toList, Var(seq.size)),
        //getAllStepsWithString(list),
        div(className:= "div_canvas"),

        renderBottomBar(),
        sliderValue.signal --> (newV => println("main: "+ newV))
    )








  def inputForString(valueSignal: Signal[String],
      valueUpdater: Observer[String]): Input =
    input(
      typ := "text",
      value <-- valueSignal,
      onInput.mapToValue --> valueUpdater,
    )
  end inputForString

  def inputForDouble(valueSignal: Signal[Double],
      valueUpdater: Observer[Double]): Input =
    val strValue = Var[String]("")
    input(
      typ := "text",
      value <-- strValue.signal,
      onInput.mapToValue --> strValue,
      valueSignal --> strValue.updater[Double] { (prevStr, newValue) =>
        if prevStr.toDoubleOption.contains(newValue) then prevStr
        else newValue.toString
      },
      strValue.signal --> { valueStr =>
        valueStr.toDoubleOption.foreach(valueUpdater.onNext)
      },
    )
  end inputForDouble

  def inputForInt(valueSignal: Signal[Int],
      valueUpdater: Observer[Int]): Input =
    input(
      typ := "text",
      controlled(
        value <-- valueSignal.map(_.toString),
        onInput.mapToValue.map(_.toIntOption).collect {
          case Some(newCount) => newCount
        } --> valueUpdater,
      ),
    )
  end inputForInt
end Main
