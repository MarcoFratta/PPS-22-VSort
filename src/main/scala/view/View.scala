package view

import com.raquo.laminar.api.L.{Element, canvasTag, child, children, className, div, li, onMountCallback, render, ul}
import controller.{Controller, ControllerImpl, Properties, PropertiesImpl}
import model.{Algorithm, ElementInfo, HasName, Params, State}
import model.InputType.*
import org.scalajs.dom

trait View:
  def getAppElement: Element
  def setSeqList(seq: Seq[Seq[ElementInfo[Int]]]): GraphFunctions
  def setProperties[X, Y](map: (X,Y)): Unit

class ViewImpl(controller: Controller) extends View:
 // val graphFunctions = GraphFunctions(controller)
  import BottomBar.*

  var properties = PropertiesImpl(controller.getAlgorithms.head, controller.getDistribution.head, Map())
  val algo = MultipleListFactory(controller.getAlgorithms)
  val dis = MultipleListFactory(controller.getDistribution)
  val params = controller.getDistribution.head.params.map(a =>
    SingleValueFactory(a)).toList




  override def getAppElement: Element =
    div(
        div(

          ul(className := "topBar",
            li(algo.element),
            li(dis.element),
            div(
              onMountCallback { divNode =>
                // Aggiungi gli elementi li all'elemento div durante il montaggio
                params.foreach { item =>
                  divNode.thisNode.ref.appendChild(li(item.element).ref)
                }
              }
            )
            //div(params.map(a => a.element).map(a => li(a)))

          //AlgTopBar(controller).getElements,
          //RenderElements(this).ulElement
          //DistributionTopBar(controller).getElements,
          )
        ),
        div(canvasTag(
          className := "canvas")),
        div(className:= "bottomBar")
       // BottomBar(controller).renderBottomBar()
    )
  override def setSeqList(seq: Seq[Seq[ElementInfo[Int]]]): GraphFunctions =
    GraphFunctions(seq)

  //private def findParamFromName(name: String): Params =
  //  properties.map.filter(a => a._1.toString equals name).toList.head._1

  override def setProperties[X, Y](map: (X, Y)): Unit =
    map match
      case a if map._1 == "Algorithm" =>
        properties.alg = controller.getAlgorithms.filter(a => a.name == map._2).head
        controller.setProperties(properties)
      case b if map._1 == "Distribution" =>
        properties.distribution = controller.getDistribution.filter(a => a.name == map._2).head
        controller.setProperties(properties)
      case _ =>
        //properties.map = properties.map.updated(map._1, map._2)
        controller.setProperties(properties)
