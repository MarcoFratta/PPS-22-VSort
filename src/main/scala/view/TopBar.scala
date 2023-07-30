
package view

import com.raquo.laminar.api.L
import model.HasName
import model.{Algorithm, Algorithms, Distribution, ElementInfo, HasName, InputType, IntModelImpl, Params, State}

import scala.collection.immutable.List
import scala.collection.immutable.List
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import controller.{Controller, ControllerImpl, Properties, PropertiesImpl}
import model.InputType.SelectList
import org.scalajs.dom
import view.BottomBar

import scala.annotation.tailrec

trait ViewElement:
  def element:Element
trait Inputs[X,Y]:
  def get:Map[X,Y]

case class MultipleList[X <: HasName, Y >: HasName](x:Set[X], selected: X) extends ViewElement with Inputs[X,Y]:
  private val selectedVar: Var[String] = Var(selected.name)
  val map: Map[String, X] = x.map(a => (a.name, a)).toMap
  def renderSelectList[T <: HasName](l: List[T]): Element =
    form(
      select(
        value <-- selectedVar.signal,
        l.map(a => option(a.name)).toList,
        inContext(thisNode =>
          onChange.mapToValue --> selectedVar,
        )
      )
    )
  override def get: Map[X, Y] = Map(map(selectedVar.now()) -> map(selectedVar.now()))
  override def element: Element = renderSelectList(x.toList)

case class SingleValue[X,Y >: Int](x:X, starterValue: Y) extends ViewElement with Inputs[X,Y]:
  val sliderValue = Var(starterValue)
  def renderSlider[T](min: Int, max: Int, item: T): Element =
    div(
      label(item.toString),
      input(
        className := "slider",
        typ := "range",
        minAttr := min.toString,
        maxAttr := max.toString,
        value := sliderValue.now().toString,
        onInput.mapToValue.map(_.toInt) --> sliderValue,
      ),
      child.text <-- sliderValue.signal.map(_.toString),
    )
  override def get: Map[X, Y] = Map(x -> sliderValue.now())
  override def element: Element = renderSlider(1, 200,  x)


object MultipleListFactory:
  def apply[X <: HasName, Y >: HasName](x: Set[X], selected: X):
  MultipleList[X, Y] =
    new MultipleList[X, Y](x, selected)

object SingleValueFactory:
  def apply[X, Y >: Int](x: X, starterValue: Y):
  SingleValue[X, Y] =
    new SingleValue[X, Y](x, starterValue: Y)

  /*def renderParamsTopBar(list: Map[Params, Int]): Unit =
  renderElement(li(list.map(a => renderSlider(0, 200, a._1.toString, a._2)).toList))
*/

/*
case class RenderElements(view: View):
  val ulElement: Element = ul()
  def renderElement(element: Element): Unit =
    render(dom.document.getElementsByClassName("topBar").item(0), element)
*/
  /*def getAlgTopBar(list: List[Algorithm[Int, Seq[State[ElementInfo[Int]]]] with HasName] ) : Element =
    //renderElement(li(renderSelectList("Algorithms", list)))
    li(renderSelectList("Algorithms", list))

  def getDisTopBar(list: List[Distribution[Int, Int] with HasName]): Element =
    //renderElement(li(renderSelectList("Distribution", list)))
    li(renderSelectList("Distribution", list))

*/




  /*var properties = PropertiesImpl(algorithmsList.head,
    distributionList.head, defaultMap)
  renderTopBar()

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


  private def renderTopBar(): Unit =
    //if dom.document.querySelector(".topBar").innerHTML != "" then
    //  dom.document.querySelector(".topBar").innerHTML = ""
    render(dom.document.querySelector(".topBar"),
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




  private def renderOption(options: List[String]): List[Element] =
    options.map(a => option(a))


  private def renderArrayProperties(name: String): Element =
    input(
      typ := "text",
      placeholder := name,
      onInput --> (v => controller.setProperties(addProperties(name ,v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement]
        .value.toInt))),
    )
*/
