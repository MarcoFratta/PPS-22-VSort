package view.livechart

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.{onClick, *}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import controller.Graphic.{play, stop}
import jdk.jfr.Enabled
import org.scalajs.dom
import view.livechart.BottomBar.renderButton
object BottomBar:
  import controller.Graphic.*

  private var backDisable = Var(true)
  private var nextDisable = Var(false)

  private var playButton = renderButton("play", "fa-play", _ =>  play(), nextDisable )
  private var stopButton = renderButton("stop", "fa-stop", _ => stop(), nextDisable)

  var controlButton = Var(renderButton("control", "fa-play", _ => if !nextDisable.now() then play(), nextDisable))
  def renderBottomBar() : Element =

    ul(
      li(renderButton("replay", "fa-rotate-left", (_) => replay(), Var(false))),
      li(renderButton("back","fa-backward", _ => backStep(), backDisable)),
      li(child <-- controlButton.signal),
      li(renderButton( "next","fa-forward", _ => nextStep(), nextDisable)),
      li(renderSpeedBar())
    )


  def renderButton(id: String, icon: String, function: (Any) => Unit, buttonDisabled: Var[Boolean]): Element =
    button(
      className := "fa " + icon,
      idAttr := id,
      onClick --> function,
      disabled <-- buttonDisabled.signal
    )


  def renderSpeedBar(): Element =
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
        //sliderValue.signal --> (newV => setSpeed(newV)),
        onInput.mapToValue.map(_.toInt) --> sliderValue,
        onInput --> (v => setSpeed(v.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value.toInt))

      ),
      child.text <-- sliderValue.signal.map(_.toString),
    )
  def changePlayIcon(): Unit =
    controlButton.set(stopButton)


  def changeStopIcon(): Unit =
    controlButton.set(playButton)

  def enableBackButton(enabled: Boolean): Unit =
    backDisable.set(!enabled)

  def disableNextButton(disable: Boolean): Unit =
    nextDisable.set(disable)

