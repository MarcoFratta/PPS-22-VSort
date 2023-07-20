package view

import com.raquo.laminar.api.L.{Element, canvasTag, className, div}
import controller.{Controller, SeqPropertiesController, SeqPropertiesControllerImpl}
import view.livechart.{BottomBar, TopBar}
import view.livechart.BottomBar.renderBottomBar


trait View:
  def getAppElement: Element
case class ViewImpl(controller: Controller) extends View:
  import BottomBar.*
  override def getAppElement: Element =
    div(
        TopBar(controller).renderTopBar(),
        div(canvasTag(
          className := "canvas")),
        renderBottomBar()
    )
