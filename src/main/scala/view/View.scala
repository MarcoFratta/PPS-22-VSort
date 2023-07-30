package view

import com.raquo.laminar.api.L.{Element, button, canvasTag, child, children, className, div, i, li, nodeSeqToModifier, onClick, onLoad, onMountBind, onMountCallback, onMountInsert, render, renderOnDomContentLoaded, ul, windowEvents}
import com.raquo.laminar.api.eventPropToProcessor
import controller.{Controller, ControllerImpl, Properties, PropertiesImpl}
import model.{Algorithm, ElementInfo, HasName, Params, State}
import model.InputType.*
import org.scalajs.dom

trait View:
  def getAppElement: Element
  def setSeqList(): GraphFunctions

class ViewImpl(controller: Controller) extends View:
 // val graphFunctions = GraphFunctions(controller)
  import BottomBar.*
  val distributions = controller.getDistribution.toList
  val algorithms = controller.getAlgorithms.toList
  var properties = controller.getProperties
  val algo = MultipleListFactory(algorithms.toSet, properties.alg)
  val dis = MultipleListFactory(distributions.toSet, properties.distribution)
  val params: List[SingleValue[Params, Int]] = properties.paramMap.map(a =>
    SingleValueFactory(a._1, a._2)).toList
  //replaceView
  //setSeqList()
  //dom.window.onload = _ => setSeqList()
  dom.document.getElementById("app").innerHTML = ""
  render(
    dom.document.getElementById("app"),
    getAppElement
  )
  replaceView
  //windowEvents(_.onLoad).foreach {_ => setSeqList()}(unsafeWindowOwner)

  def replaceView: Unit =
    println("replacing view")
    //dom.document.getElementById("app").innerHTML = ""
    //render(dom.document.getElementById("app"), getAppElement)

    dom.document.getElementById("app").addEventListener("onChange", _ =>  setSeqList())



  override def getAppElement: Element =
    println("getappelem")
    div(
        div(

          ul(className := "topBar",
            li(algo.element),
            li(dis.element),
            params.map(a => li(a.element)),
            /*
            div(
              onMountBind { divNode =>
                // Aggiungi gli elementi li all'elemento div durante il montaggio
                params.map { item =>
                  divNode.thisNode.ref.appendChild(li(item.element).ref)
                }
              }
            ),
            */
            li(
              button(
                i(
                  className := "fa fa-check",
                  onClick --> (_ =>
                    println("click")
                    new ControllerImpl(new PropertiesImpl(alg = algo.get.head._1, distribution = dis.get.head._1,
                      paramMap = params.map(a => a.get).toList.foldLeft(Map.empty[Params, Int]) { (acc, map) =>
                        acc ++ map
                      } ))

                    )
                )
              )
            )
            //div(params.map(a => a.element).map(a => li(a)))

          //AlgTopBar(controller).getElements,
          //RenderElements(this).ulElement
          //DistributionTopBar(controller).getElements,
          )
        ),
        onMountCallback (_ => setSeqList()),
        div(canvasTag(
          className := "canvas")),
        div(className:= "bottomBar"),
       // BottomBar(controller).renderBottomBar()

    )

  def computeUl: Element =
    div(params.map(a => li(a.element)))

  override def setSeqList(): GraphFunctions =
    println("setSeqList")
    GraphFunctions(controller.getSeqElement)

  //private def findParamFromName(name: String): Params =
  //  properties.map.filter(a => a._1.toString equals name).toList.head._1


