import com.raquo.laminar.api.L.renderOnDomContentLoaded
import controller.*
import model.{ControllerComponent, IntTypes, ModelComponent, ViewComponent}
import org.scalajs.dom
import view.*



@main
def Main(): Unit =

  println("Creating MVC")
  object MVC
      extends ModelComponent.Model.Interface
        with ViewComponent.Interface
        with ControllerComponent.Interface:
      // Instantiation of components , dependencies are implicit
      override val model = new ModelImpl()
      override val view = new ViewImpl()
      override val controller = new ControllerImpl()
  MVC.controller.update(Properties(MVC.model.algorithms.head, MVC.model.distributions.head,
    MVC.model.distributions.head.params.map(a => a -> 10).toMap))



