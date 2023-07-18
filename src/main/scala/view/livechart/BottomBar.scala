package view.livechart

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.{onClick, *}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import jdk.jfr.Enabled
import org.scalajs.dom
import view.livechart.BottomBar
import view.rectangles.GraphFunctions.*

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

class ButtonImpl(override val id:String, override val icon: String, override val function: Any => Unit,
                 override val buttonDisabled: Var[Boolean]) extends Button

object BottomBar:
  private val nextDisable = Var(true)
  private val backDisable = Var(true)
  private val controlButton = Var(new ButtonImpl("controlButton", "fa-play", _ => play(), nextDisable).getButton)
  private val playButton = new ButtonImpl("play", "fa-play", _ => play(), nextDisable)
  private val stopButton = new ButtonImpl("stop", "fa-stop", _ => stop(), nextDisable)

  def renderBottomBar() : Element =
    ul(
      li(new ButtonImpl("replay", "fa-rotate-left", _ => replay(), nextDisable).getButton),
      li(new ButtonImpl("back", "fa-backward", _ => backStep(), backDisable).getButton),
      li(child <-- controlButton.signal),
      li(new ButtonImpl("next", "fa-forward", _ => nextStep(), nextDisable).getButton),
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
        onInput --> (v => setSpeed(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt))

      ),
      child.text <-- sliderValue.signal.map(_.toString),
    )

  def enableAllButton(): Unit =
    nextDisable.set(false)
  
  def changePlayIcon(): Unit =
    controlButton.set(stopButton.getButton)

  def changeStopIcon(): Unit =
    controlButton.set(playButton.getButton)

  def enableBackButton(enabled: Boolean): Unit =
    backDisable.set(!enabled)

  def disableNextButton(disable: Boolean): Unit =
    nextDisable.set(disable)

