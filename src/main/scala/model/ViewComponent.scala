package model

import com.raquo.laminar.api.L
import controller.Properties
import model.*
import org.scalajs.dom
import view.{BottomBar, GraphicVisualizer, MultipleListFactory, MultipleListWithFFactory, SingleValue, SingleValueFactory}
import com.raquo.laminar.api.L.{Element, button, canvasTag, child, children, className, div, i, li, nodeSeqToModifier, onClick, onLoad, onMountBind, onMountCallback, onMountInsert, render, renderOnDomContentLoaded, ul, windowEvents}
import com.raquo.laminar.api.eventPropToProcessor



object ViewComponent:
  trait View extends ModelTypes:
    def update(data: ResultType): Unit
  trait Provider:
    val view: View with IntTypes

  type Requirements = ModelComponent.Model.Provider with ControllerComponent.Provider
  trait Component:
    c:Requirements =>

    class ViewImpl extends View with IntTypes:

      private var gui:JsView = JsView(Seq(), c.model.algorithms.toList, c.model.distributions.toList,
        Properties(c.model.algorithms.toList.head, c.model.distributions.toList.head,
          c.model.distributions.toList.head.params.map(a => a -> 10).toMap))
      override def update(data: c.model.ResultType): Unit = gui = gui.updated(data)
      private case class JsView(data:c.model.ResultType,
                                algorithms:List[Algorithm[c.model.ValType,c.model.ResultType] with HasName],
                                distributions:List[Distribution[c.model.ParamsType,c.model.ValType] with HasName],
                                p:Properties with IntTypes):
        import BottomBar.*

        println("Entering JsView")
        private val algo = MultipleListFactory(algorithms, p.algorithm)
        private var selectedP = p
        println("selectedp " + selectedP.params)
        private val dis = MultipleListWithFFactory(distributions, p.distribution,
          x =>
            val parameterMap = distributions.find(a => a equals x).get.params.map(a => a -> 10).toMap
            selectedP = Properties(algo.get.head._1,x, parameterMap)
            c.controller.update(selectedP)
        )

        given Conversion[c.model.ParamsType, Int] = x => x
        private val params = p.params.map(a => SingleValueFactory(a._1, a._2, a._1.min, a._1.max)).toList

        dom.document.getElementById("app").innerHTML = ""
        render(dom.document.getElementById("app"), getAppElement)
        replaceView()

        private def replaceView(): Unit =
          dom.document.getElementById("app").addEventListener("onChange", _ =>  updated(data))

        private def getAppElement: Element =
          div(
            div(
              ul(className := "topBar",
                li(algo.element),
                li(dis.element),
                params.map(a => li(a.element)),
                li(
                  button(
                    className := "fa fa-check",
                    onClick --> (_ =>
                      val paramMap = params.map(a => a.get).foldLeft(Map.empty[Params, ParamsType])((param, map) => param ++ map)
                      selectedP = Properties(algo.get.head._1, dis.get.head._1, paramMap)
                      c.controller.update(selectedP)
                    )
                  )
                )
              )
            ),
            onMountCallback (_ =>
              if data.nonEmpty then GraphicVisualizer(data)),
            div(canvasTag(
              className := "canvas")),
            div(className:= "bottomBar"),
          )

        def updated(data: c.model.ResultType): JsView =
          copy(data = data, p = selectedP)

  trait Interface extends Provider with Component:
    self: Requirements =>