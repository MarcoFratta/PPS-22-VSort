package controller.component

import model.component.ModelComponent.Model
import model.component.{IntTypes, ModelComponent, Params}
import view.*
import view.component.ViewComponent

object ControllerComponent:


  trait Controller[T]:
    def update(p: T): Unit

  trait Provider:
    val controller: Controller[Properties with IntTypes]

  type Requirements = ModelComponent.Provider with ViewComponent.Provider
  trait Component:
    c: Requirements =>

    class ControllerImpl extends Controller[Properties with IntTypes]:
      import Params.*
      override def update(p: Properties with IntTypes): Unit =
        val r = c.model.getData(p)
        if checkField(p) then c.view.update(r)

      private def checkField(properties: Properties with IntTypes): Boolean =
        properties.algorithm != null && properties.distribution != null && properties.params.nonEmpty


  trait Interface extends Provider with Component:
    self: Requirements =>
