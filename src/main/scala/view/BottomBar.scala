package view

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import jdk.jfr.Enabled
import org.scalajs.dom
import view.GraphicVisualizer.*
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

object BottomBar:
 // private val graphFunctions = GraphFunctions(controller)
  private val nextDisable = Var(false)
  private val backDisable = Var(true)
  private val replayDisable = Var(false)
  private val controlButton = Var( ButtonImpl("controlButton", "fa-play", _ => GraphicVisualizer.play(), nextDisable).getButton)
  private val playButton = ButtonImpl("play", "fa-play", _ => GraphicVisualizer.play(), nextDisable)
  private val stopButton = ButtonImpl("stop", "fa-stop", _ => GraphicVisualizer.stop(), nextDisable)

  def getBottomBar : Element =
    ul(
      li(ButtonImpl("replay", "fa-rotate-left", _ => GraphicVisualizer.replay(), replayDisable).getButton),
      li(ButtonImpl("back", "fa-backward", _ => GraphicVisualizer.backStep(), backDisable).getButton),
      li(child <-- controlButton.signal),
      li(ButtonImpl("next", "fa-forward", _ => GraphicVisualizer.nextStep(), nextDisable).getButton),
      li(renderSpeedBar)
    )

  private def renderSpeedBar: Element =
    val sliderValue: Var[Int] = Var(800)
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
        onChange.mapToValue.map(_.toInt) --> sliderValue,
        onChange --> (v => GraphicVisualizer.setSpeed(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt))

      ),
    )


  def changePlayIcon(): Unit =
    controlButton.set(stopButton.getButton)

  def changeStopIcon(): Unit =
    controlButton.set(playButton.getButton)

  def enableBackButton(enabled: Boolean): Unit =
    backDisable.set(!enabled)

  def disableNextButton(disabled: Boolean): Unit =
    nextDisable.set(disabled)


