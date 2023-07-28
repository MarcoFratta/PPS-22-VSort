package view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import controller.{Controller, Properties, PropertiesImpl}
import model.{Algorithms, Distribution, HasName, InputType, IntModelImpl, Params}
import org.scalajs.dom
import view.BottomBar

import scala.annotation.tailrec

case class TopBar(controller: Controller):
  var properties = PropertiesImpl(IntModelImpl().algorithms.toList.head,
    IntModelImpl().distributions.toList.head, defaultMap)

  private def defaultMap: Map[Params, Int] =
    Map(Params.Size -> 50)
  private def computeInputList: List[InputType] =
    val list: List[InputType] = List(InputType.SelectList("Algorithm", controller.getAlgorithms),
      InputType.SelectList("Distribution", controller.getDistribution)).
      concat(computeVariableInputList(controller.getDistribution.head))
    list

  private def computeVariableInputList(distribution: Distribution[Int, Int]): List[InputType] =
    distribution.params.foreach(a => properties.map = properties.map.updated(a, -1))
    properties.map.map(a => InputType.Slider(0,200,a._1.toString,a._2)).toList
    //list = list.appended(InputType.Slider(10, 100, "Size", 50))

  def replaceTopBar(list: List[InputType]): Unit =
    dom.document.querySelector(".varInputList").innerHTML = ""
    render(dom.document.querySelector(".varInputList"), div(list.map(a => li(renderInputType(a)))))


  def renderTopBar(): Element =
    ul(

      computeInputList.map(a => li(renderInputType(a))),
      /*div(
        className := "varInputList",
        controller.getVariableInputList.map(a => li(renderInputType(a)))),
      */
      li(
        button (
          i(
            className:="fa fa-check",
            onClick --> (_ =>
              controller.setProperties(properties))
          )
        )
      )
    )
  private def renderInputType(inputType: InputType): Element =
    inputType match
      case InputType.SelectList(name, l) => renderSelectList(name, l)
      case InputType.Text(text) => renderArrayProperties(text)
      case InputType.Slider(min, max, name, defaultValue) => renderSlider(min, max, name, defaultValue)

  private def addProperties(name: String, value: Int): Properties =
    properties.map = properties.map.updated(findParamFromName(name), value)
    println("aggiornata mappa: " + properties.map)
    properties

  private def findParamFromName(name: String): Params =
    properties.map.filter(a => a._1.toString equals name).toList.head._1

  private def renderSlider(min: Int, max: Int, name: String, defaultValue: Int): Element =
    val sliderValue = Var(defaultValue)
    div(
      label(name),
      input(
        className := "slider",
        typ := "range",
        minAttr := min.toString,
        maxAttr := max.toString,
        value:= sliderValue.now().toString,
        onInput --> (v =>
          controller.setProperties(addProperties(name ,v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt))),
        //onInput --> (v => f(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt)),
        onInput.mapToValue.map(_.toInt) --> sliderValue

      ),
        child.text <-- sliderValue.signal.map(_.toString),
      )

  private def renderSelectList[T <: HasName](name: String, l:  List[T]): Element =
    form(
      select(
        label(name),
          l.map(a => option(a.name)).toList,
        onChange --> (v =>
          println("selected")
          controller.setProperties(addProperties(name, v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt))),
      )
    )
  private def renderOption(options: List[String]): List[Element] =
    options.map(a => option(a))
  

  private def renderArrayProperties(name: String): Element =
    input(
      typ := "text",
      placeholder := name,
      onInput --> (v => controller.setProperties(addProperties(name ,v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement]
        .value.toInt))),
    )

