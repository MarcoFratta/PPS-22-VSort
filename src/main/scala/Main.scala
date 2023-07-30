import com.raquo.laminar.api.L.renderOnDomContentLoaded
import controller.{Controller, ControllerImpl, PropertiesImpl}
import model.{IntModel, IntModelImpl}
import org.scalajs.dom
import view.{View, ViewImpl}

@main
def Main(): Unit =
  val model: IntModel = IntModelImpl()
  val controller: Controller = ControllerImpl(PropertiesImpl(IntModelImpl().algorithms.head,
    IntModelImpl().distributions.head, IntModelImpl().distributions.head.params.map(el => el -> 50).toMap))
  val view: View = ViewImpl(controller)

