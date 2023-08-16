package view

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import controller.component.Properties
import model.*
import model.component.HasName
import org.scalajs.dom
import view.BottomBar
import view.InputType.SelectList

import scala.annotation.tailrec
import scala.collection.immutable.List

trait ViewElement:
  def element:Element
trait Inputs[X,Y]:
  def get:Map[X,Y]

trait InputElement[X,Y] extends ViewElement with Inputs[X,Y]

trait MultipleList[X <: HasName, Y >: HasName](x:List[X], selected: X) extends InputElement[X,Y]:

  val selectedVar: Var[String] = Var(selected.name)
  val map: Map[String, X] = x.map(a => (a.name, a)).toMap
  private def renderSelectList[T <: HasName](l: List[T]): Element =
    form(
      select(
        value <-- selectedVar.signal,
        l.map(a => option(a.name)).toList,
        inContext(thisNode =>
          onChange.mapToValue --> selectedVar,
        )
      )
    )
  override def get: Map[X, Y] = Map(map(selectedVar.now()) -> map(selectedVar.now()))
  override def element: Element = renderSelectList(x)

case class MultipleListImpl[X <: HasName, Y >: HasName](x:List[X], selected: X) extends MultipleList[X,Y](x, selected)
case class MultipleListWithF[X <: HasName, Y >: HasName](x:List[X], selected: X, f: X => Unit)
  extends MultipleList[X,Y](x, selected):
  override def element: L.Element =
    form(
    select(
      value <-- selectedVar.signal,
      x.map(a => option(a.name)).toList,
      onChange.mapToValue --> selectedVar,
      onChange.mapToValue --> (a => f(map(selectedVar.now()))),
    )
  )


case class SingleValue[X,Y >:Int](x:X, starterValue: Y, min: Y, max: Y) extends InputElement[X,Y]:
  val sliderValue = Var(starterValue)
  private def renderSlider[T](item: T): Element =
    div(
      label(item.toString),
      input(
        className := "slider",
        typ := "range",
        minAttr := min.toString,
        maxAttr := max.toString,
        value := sliderValue.now().toString,
        onInput.mapToValue.map(_.toInt) --> sliderValue,
      ),
    )
  override def get: Map[X, Y] = Map(x -> sliderValue.now())
  override def element: Element = renderSlider(x)


object MultipleListFactory:
  def apply[X <: HasName, Y >: HasName](x: List[X], selected: X):
  MultipleList[X, Y] = MultipleListImpl[X, Y](x, selected)

object MultipleListWithFFactory:
  def apply[X <: HasName, Y >: HasName](x: List[X], selected: X, f: X => Unit):
  MultipleListWithF[X, Y] = MultipleListWithF[X, Y](x, selected, f)

object SingleValueFactory:
  def apply[X, Y](x: X, starterValue: Y, min: Y, max: Y)(using c:Conversion[Y,Int]):
  SingleValue[X, Int] =
    new SingleValue[X, Int](x, c.apply(starterValue), min, max)
