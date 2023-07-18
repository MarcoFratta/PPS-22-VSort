package view.livechart

import com.raquo.laminar.api.L.*
import controller.{StepController, SeqPropertiesController}
import view.livechart.Main.sliderValue
import org.scalajs.dom
import view.rectangles.GraphFunctions.changeSize


  def renderTopBar(sliderValue: Var[Int]): Element =
    ul(
      li(renderSelectionAlg()),
      li(renderSelectionDistribution()),
      li(renderArrayProperties("Max")),
      li(renderArrayProperties("Min")),
      li(renderArrayProperties("Size")),
      li(renderArrayProperties("% valori duplicati")),
      li(renderSlider(sliderValue)),
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
    
  
  import controller.*
  def renderSlider(sliderValue: Var[Int]): Element =
    //val slidVal = Var(sliderValue.now())

    div(
    input(
      className := "slider",
      typ := "range",
      minAttr := "2",
      maxAttr := "200",
      value:= sliderValue.now().toString,
      onInput --> (v => changeSize(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt)),
      onInput.mapToValue.map(_.toInt) --> sliderValue

    ),

      child.text <-- sliderValue.signal.map(_.toString),


    )

  def setAlgNames(l: List[String]): Element = l match
    case h :: t =>
      dom.document.getElementById("algList").appendChild(option(h).ref)
      setAlgNames(t)
  def renderSelectionAlg(): Element =
    form(
    select(
      idAttr := "algList",
      placeholder("Algorithm"),
        //renderOption(n)
      )
    )

  def renderSelectionDistribution(): Element =
    form(
      select(
        placeholder("Distribution"),
        option("Random"),
        option("Gaussian")
      )
    )

  def renderArrayProperties(name: String): Element =
    input(
      typ := "text",
      placeholder := name
    )

