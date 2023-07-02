package view.livechart

import com.raquo.laminar.api.L.*
import view.livechart.Main.sliderValue

object TopBar:

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
            className:="fa fa-check"
          )
        )
      )
    )

  def renderSlider(sliderValue: Var[Int]): Element =
    //val slidVal = Var(sliderValue.now())

    div(
    input(
      className := "slider",
      typ := "range",
      minAttr := "0",
      maxAttr := "200",

      sliderValue.signal --> (newV => println("topBar: "+ newV.toString)),

      onInput.mapToValue.map(_.toInt) --> sliderValue

    ),

      child.text <-- sliderValue.signal.map(_.toString),


    )
  def renderSelectionAlg(): Element =
    form(
      select(
        placeholder("Algorithm"),
        option("bubble sort"),
        option("heap sort")
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

