package view

import com.raquo.laminar.api.L.{Element, canvasTag, className, div}
import view.livechart.{BottomBar, renderTopBar}
import view.livechart.BottomBar.renderBottomBar

object MainView:
  import BottomBar.*
  import view.rectangles.*
  def appElement(): Element =
    div(
        renderTopBar(),
        div(canvasTag(
          className := "canvas")),
        renderBottomBar()
    )