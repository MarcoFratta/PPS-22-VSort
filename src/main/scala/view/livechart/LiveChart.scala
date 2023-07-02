package view.livechart

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import model.SeqProperties.Generators.{exponentialDistribution, normalDistribution, uniformDistribution}
import model.SeqProperties.Setters.*
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
  val model = new Model
  val minValue = 120
  val sliderValue = Var(minValue)
  import BottomBar.*
  import TopBar.*
  import model.*
  import view.rectangles.*



  def appElement(): Element =
    val data = uniformDistribution(0,100).take(50).map(a => a.toInt)
    val steps = bubbleSort(data)
    val list = getMapList(steps, data).map(l => l.map((v, _) => v))
    val allSeq = list.map(a => a.toSeq)
    val seq = normalDistribution(50, 50).take(sliderValue.signal.now()).shift(1, 200).doubleToInt
    seq.foreach(println(_))
    //val seq = exponentialDistribution(50, 200).take(200)
    //seq.foreach(println(_))
    //val seq = Seq(1,2,4)
    div(
        renderTopBar(sliderValue),
        // renderDataTable(),
        //renderDataList(),
        getAllSteps(allSeq),
        renderBottomBar(),
        sliderValue.signal --> (newV => println("main: "+ newV))
  )
  end appElement




  def renderDataList(): Element =
    ul(
      children <-- dataSignal.split(_.id) { (id, initial, itemSignal) =>
        li(child.text <-- itemSignal.map(item => s"${item.count} ${item.label}"))
      }
    )
  end renderDataList

  def renderDataTable(): Element =
    table(
      thead(tr(th("Label"), th("Price"), th("Count"), th("Full price"), th("Action"))),
      tbody(
        children <-- dataSignal.split(_.id) { (id, initial, itemSignal) =>
          renderDataItem(id, itemSignal)
        },
      ),
      tfoot(tr(
        td(button("➕", onClick --> (_ => addDataItem(DataItem())))),
        td(),
        td(),
        td(child.text <-- dataSignal.map(data => "%.2f".format(data.map(_.fullPrice).sum))),
      )),
    )
  end renderDataTable

  def renderDataItem(id: DataItemID, itemSignal: Signal[DataItem]): Element =
    tr(
      td(
        inputForString(
          itemSignal.map(_.label),
          makeDataItemUpdater(id, { (item, newLabel) =>
            item.copy(label = newLabel)
          })
        )
      ),
      td(
        inputForDouble(
          itemSignal.map(_.price),
          makeDataItemUpdater(id, { (item, newPrice) =>
            item.copy(price = newPrice)
          })
        )
      ),
      td(
        inputForInt(
          itemSignal.map(_.count),
          makeDataItemUpdater(id, { (item, newCount) =>
            item.copy(count = newCount)
          })
        )
      ),
      td(
        child.text <-- itemSignal.map(item => "%.2f".format(item.fullPrice))
      ),
      td(button("🗑️", onClick --> (_ => removeDataItem(id)))),
    )
  end renderDataItem

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
