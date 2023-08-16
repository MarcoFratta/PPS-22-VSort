package view.component

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.eventPropToProcessor
import controller.component.{ControllerComponent, Properties}
import model.*
import model.component.*
import org.scalajs.dom
import org.scalajs.dom.KeyFormat.raw
import view.*



object ViewComponent:
  trait View extends ModelTypes:
    def update(data: ResultType): Unit
  trait Provider:
    val view: View with IntTypes

  type Requirements = ControllerComponent.Provider with ModelComponent.Observer
  trait Component:
    c:Requirements =>

    class ViewImpl extends View with IntTypes:

      private var gui: JsView = JsView(Seq(),
        c.viewModel.algorithms.toList,
        c.viewModel.distributions.toList,
        Properties.defaultProperty(c.viewModel))

      override def update(data: c.viewModel.ResultType): Unit = gui = gui.updated(data)

      private case class JsView(data: c.viewModel.ResultType,
                                algorithms: List[Algorithm[c.viewModel.ValType, c.viewModel.ResultType] with HasName],
                                distributions: List[Distribution[c.viewModel.ParamsType, c.viewModel.ValType] with HasName],
                                p: Properties with IntTypes):
        private val DefaultValue = 10

        import BottomBar.*

        private val algo = MultipleListFactory(algorithms, p.algorithm)
        private var selectedP = p
        private val dis = MultipleListWithFFactory(distributions, p.distribution,
          x =>
            val parameterMap = distributions.find(a => a equals x).get.params.map(a => a -> DefaultValue).toMap
            selectedP = Properties(algo.get.head._1, x, parameterMap)
            c.controller.update(selectedP)
        )

        given Conversion[c.viewModel.ParamsType, Int] = x => x
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
              if data.nonEmpty then GraphicVisualizer.setData(data)),
            div(canvasTag(
              className := "canvas")),
            div(className:= "bottomBar",
              BottomBar.getBottomBar),
          )

        def updated(data: c.viewModel.ResultType): JsView =
          copy(data = data, p = selectedP)

  trait Interface extends Provider with Component:
    self: Requirements =>