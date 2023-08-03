
package view

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveElement
import controller.Properties
import model.InputType.SelectList
import model.*
import org.scalajs.dom
import view.BottomBar

import scala.annotation.tailrec
import scala.collection.immutable.List

trait ViewElement:
  def element:Element
trait Inputs[X,Y]:
  def get:Map[X,Y]

trait MultipleList[X <: HasName, Y >: HasName](x:Set[X], selected: X) extends ViewElement with Inputs[X,Y]:

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
  override def element: Element = renderSelectList(x.toList)

case class MultipleListImpl[X <: HasName, Y >: HasName](x:Set[X], selected: X) extends MultipleList[X,Y](x, selected)
case class MultipleListWithF[X <: HasName, Y >: HasName](x:Set[X], selected: X, f: X => Unit)
  extends MultipleList[X,Y](x, selected):
  override def element: L.Element =
    form(
    select(
      value <-- selectedVar.signal,
      x.toList.map(a => option(a.name)).toList,
      onChange.mapToValue --> selectedVar,
      onChange.mapToValue --> (a => f(map(selectedVar.now()))),
    )
  )

trait IntConverter[T]:
  def convert(x:T):Int

case class SingleValue[X,Y >:Int](x:X, starterValue: Y) extends ViewElement with Inputs[X,Y]:
  val sliderValue = Var(starterValue)
  def renderSlider[T](min: Int, max: Int, item: T): Element =
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
  override def element: Element = renderSlider(1, 200,  x)


object MultipleListFactory:
  def apply[X <: HasName, Y >: HasName](x: Set[X], selected: X):
  MultipleList[X, Y] = MultipleListImpl[X, Y](x, selected)

object MultipleListWithFFactory:
  def apply[X <: HasName, Y >: HasName](x: Set[X], selected: X, f: X => Unit):
  MultipleListWithF[X, Y] = MultipleListWithF[X, Y](x, selected, f)

object SingleValueFactory:
  def apply[X, Y](x: X, starterValue: Y)(using c:Conversion[Y,Int]):
  SingleValue[X, Int] =
    new SingleValue[X, Int](x, c.apply(starterValue))
