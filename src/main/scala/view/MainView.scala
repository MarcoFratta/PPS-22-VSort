package view

import com.raquo.laminar.api.L.{Element, canvasTag, className, div}
import controller.{MainController, SeqPropertiesController, SeqPropertiesControllerImpl}
import view.livechart.{BottomBar, TopBar}
import view.livechart.BottomBar.renderBottomBar



object MainView:
  import BottomBar.*

  var seqProp: MainController = new MainController()
  def appElement(): Element =
    div(
        TopBar(seqProp).renderTopBar(),
        div(canvasTag(
          className := "canvas")),
        renderBottomBar()
    )
