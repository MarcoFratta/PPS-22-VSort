package view.livechart

import com.raquo.laminar.api.L.*

object TopBar:

  def renderTopBar(): Element =
    ul(
      li(renderSelectionAlg()),
      li(renderSelectionDistribution()),
      li(renderArrayProperties("Max")),
      li(renderArrayProperties("Min")),
      li(renderArrayProperties("Size")),
      li(renderArrayProperties("% valori duplicati")),
      li(
        button (
          i(
            className:="fa fa-check"
          )
        )
      )
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

