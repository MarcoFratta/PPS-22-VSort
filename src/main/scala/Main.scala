import com.raquo.laminar.api.L.renderOnDomContentLoaded
import controller.*
import model.{Algorithms, Distributions, IntTypes, ModelComponent}
import org.scalajs.dom
import view.*



@main
def Main(): Unit =

  println("Creating MVC")
  object MVC
    extends ModelComponent.Interface
      with ViewComponent.Interface
      with ControllerComponent.Interface:
    // Instantiation of components , dependencies are implicit
    override val model = intModel
    override val viewModel = intModel
    override val view = new ViewImpl()
    override val controller = new ControllerImpl()
    val intModel = new ModelImpl()

  MVC.controller.update(Properties.defaultProperty(MVC.viewModel))



