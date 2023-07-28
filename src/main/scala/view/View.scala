package view

import com.raquo.laminar.api.L.{Element, canvasTag, className, div}
import controller.Controller
import model.ElementInfo

trait View:
  def getAppElement: Element
  def setSeqList(seq: Seq[Seq[ElementInfo[Int]]]): GraphFunctions

class ViewImpl(controller: Controller) extends View:
 // val graphFunctions = GraphFunctions(controller)
  import BottomBar.*
  override def getAppElement: Element =
    div(
        div(
          className := "topBar",
          TopBar(controller).renderTopBar(),
        ),
        div(canvasTag(
          className := "canvas")),
        div(className:= "bottomBar")
       // BottomBar(controller).renderBottomBar()
    )
  override def setSeqList(seq: Seq[Seq[ElementInfo[Int]]]): GraphFunctions =
    GraphFunctions(seq)
