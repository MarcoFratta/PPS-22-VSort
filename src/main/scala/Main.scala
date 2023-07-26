import com.raquo.laminar.api.L.renderOnDomContentLoaded
import controller.{Controller, ControllerImpl}
import org.scalajs.dom
import view.{View, ViewImpl}

@main
def Main(): Unit =
  val controller: Controller = ControllerImpl()
  val view: View = ViewImpl(controller)
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    view.getAppElement
  )
