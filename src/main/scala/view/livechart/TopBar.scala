package view.livechart

import com.raquo.laminar.api.L.*
import controller.{MainController, SeqPropertiesController, StepController}
import model.InputType
import org.scalajs.dom
import view.rectangles.GraphFunctions.changeSize

import scala.annotation.tailrec

case class TopBar(prop: MainController):
  def renderTopBar(): Element =
    ul(
      prop.getInputList().map(a => li(renderInputType(a))),
      li(
        button (
          i(
            className:="fa fa-check",
            onClick --> (_ =>
              BottomBar.enableAllButton()
              StepController.setSeqList())
          )
        )
      )
    )
  private def renderInputType(inputType: InputType): Element =
    inputType match
      case InputType.SelectList(l) => renderSelectList(l)
      case InputType.Text(text) => renderArrayProperties(text)
      case InputType.Slider(min, max, name, defaultValue) => renderSlider(min, max, name, defaultValue)
  private def renderSlider(min: Int, max: Int, name: String, defaultValue: Int): Element =
    var sliderValue = Var(defaultValue)
    div(
    input(
      className := "slider",
      typ := "range",
      minAttr := min.toString,
      maxAttr := max.toString,
      value:= sliderValue.now().toString,
      onInput --> (v => changeSize(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt)),
      onInput.mapToValue.map(_.toInt) --> sliderValue

    ),
      child.text <-- sliderValue.signal.map(_.toString),
    )


  private def renderOption(options: List[String]): List[Element] =
    options.map(a => option(a))
  private def renderSelectList(l: List[String]): Element =
    form(
    select(
      renderOption(l)
      )
    )

  private def renderArrayProperties(name: String): Element =
    input(
      typ := "text",
      placeholder := name
    )

