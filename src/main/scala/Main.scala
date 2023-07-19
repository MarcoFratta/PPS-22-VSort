import com.raquo.laminar.api.L.renderOnDomContentLoaded
import org.scalajs.dom
import view.MainView

@main
def Main(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    MainView.appElement()
  )
