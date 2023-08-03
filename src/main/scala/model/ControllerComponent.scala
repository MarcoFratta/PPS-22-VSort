package model

import controller.Properties
import controller.StepController.SeqProperties
import model.*
import model.ModelComponent.Model
import view.*

object ControllerComponent:
  type Requirements = Model.Provider with ViewComponent.Provider

  trait Controller[T]:
    def update(p: T): Unit

  trait Provider:
    val controller: Controller[Properties with IntTypes]

  trait Component:
    c: Requirements =>

    class ControllerImpl extends Controller[Properties with IntTypes]:

      override def update(p: Properties with IntTypes): Unit =
        val r = SeqProperties(p).getElements
        //println(f"res  $r")
        if checkField(p) then c.view.update(r)


      private def checkField(properties: Properties with IntTypes): Boolean =
        properties.algorithm != null && properties.distribution != null && properties.params.nonEmpty


  trait Interface extends Provider with Component:
    self: Requirements =>
