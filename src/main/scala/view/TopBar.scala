package view

import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import controller.Controller
import model.InputType
import org.scalajs.dom
import view.BottomBar

import scala.annotation.tailrec

case class TopBar(controller: Controller):


  def replaceTopBar(list: List[InputType]): Unit =
    dom.document.querySelector(".varInputList").innerHTML = ""
    render(dom.document.querySelector(".varInputList"), div(list.map(a => li(renderInputType(a)))))
  def renderTopBar(): Element =
    ul(

      controller.getInputList.map(a => li(renderInputType(a))),
      div(
        className := "varInputList",
        controller.getVariableInputList.map(a => li(renderInputType(a)))),

      li(
        button (
          i(
            className:="fa fa-check",
            onClick --> (_ =>
              controller.sendData())
          )
        )
      )
    )
  private def renderInputType(inputType: InputType): Element =
    inputType match
      case InputType.SelectList(name, l) => renderSelectList(name, l)
      case InputType.Text(text) => renderArrayProperties(text)
      case InputType.Slider(min, max, name, defaultValue) => renderSlider(min, max, name, defaultValue)
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
        onInput --> (v => controller.addProperties(name ,v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt)),
        //onInput --> (v => f(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt)),
        onInput.mapToValue.map(_.toInt) --> sliderValue

      ),
        child.text <-- sliderValue.signal.map(_.toString),
      )


  private def renderOption(options: List[String]): List[Element] =
    options.map(a => option(a))
  private def renderSelectList[T](name: String,l: List[T]): Element =
    form(
    select(
      label(name),
      renderOption(l.map(a => a.toString)),
      onChange --> (v =>
        println("selected")
        controller.onOptionSelected(name, v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value)),

      )
    )

  private def renderArrayProperties(name: String): Element =
    input(
      typ := "text",
      placeholder := name,
      onInput --> (v => controller.addProperties(name ,v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt)),
    )

