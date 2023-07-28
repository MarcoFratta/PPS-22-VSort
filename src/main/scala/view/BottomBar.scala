package view

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import controller.Controller
import jdk.jfr.Enabled
import org.scalajs.dom
import view.GraphFunctions.*
trait Button:
  def id: String

  def icon: String

  def function: Any => Unit
  def buttonDisabled: Var[Boolean]
  
  def getButton: Element =
    button(
      className := "fa " + icon,
      idAttr := id,
      onClick --> function,
      disabled <-- buttonDisabled.signal
    )

case class ButtonImpl(override val id:String, override val icon: String, override val function: Any => Unit,
                 override val buttonDisabled: Var[Boolean]) extends Button

case class BottomBar(graphFunctions: GraphFunctions):
 // private val graphFunctions = GraphFunctions(controller)
  private val nextDisable = Var(false)
  private val backDisable = Var(true)
  private val replayDisable = Var(false)
  private val controlButton = Var( ButtonImpl("controlButton", "fa-play", _ => graphFunctions.play(), nextDisable).getButton)
  private val playButton = ButtonImpl("play", "fa-play", _ => graphFunctions.play(), nextDisable)
  private val stopButton = ButtonImpl("stop", "fa-stop", _ => graphFunctions.stop(), nextDisable)

  def renderBottomBar() : Element =
    ul(
      li(ButtonImpl("replay", "fa-rotate-left", _ => graphFunctions.replay(), replayDisable).getButton),
      li(ButtonImpl("back", "fa-backward", _ => graphFunctions.backStep(), backDisable).getButton),
      li(child <-- controlButton.signal),
      li(ButtonImpl("next", "fa-forward", _ => graphFunctions.nextStep(), nextDisable).getButton),
      li(renderSpeedBar())
    )

  private def renderSpeedBar(): Element =
    val sliderValue: Var[Int] = Var(100)
    div(
      i(
        className := "fa fa-tachometer-alt"
      ),
      input(
        className := "slider",
        typ := "range",
        minAttr := "1",
        maxAttr := "1000",
        value := sliderValue.now().toString,
        onInput.mapToValue.map(_.toInt) --> sliderValue,
        onInput --> (v => graphFunctions.setSpeed(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt))

      ),
      child.text <-- sliderValue.signal.map(_.toString),
    )
  
  
  def changePlayIcon(): Unit =
    controlButton.set(stopButton.getButton)

  def changeStopIcon(): Unit =
    controlButton.set(playButton.getButton)

  def enableBackButton(enabled: Boolean): Unit =
    backDisable.set(!enabled)

  def disableNextButton(disable: Boolean): Unit =
    nextDisable.set(disable)


