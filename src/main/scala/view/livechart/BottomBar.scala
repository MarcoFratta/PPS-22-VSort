package view.livechart

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.{onClick, *}
import com.raquo.laminar.nodes.ReactiveHtmlElement
import jdk.jfr.Enabled
import org.scalajs.dom
import view.livechart.BottomBar.renderBottomIcon
object BottomBar:
  import controller.Graphic.*

  var backDisable = Var(true)
  var nextDisable = Var(false)
  var playButton = renderBottomIcon("play", "fa-play", _ => play())
  var stopButton = renderBottomIcon("stop", "fa-stop", _ => stop())
  var controlButton = renderDisablingButton("fa-play", _ => play(), nextDisable)
  def renderBottomBar() : Element =

    ul(
      li(renderBottomIcon("", "fa-rotate-left", (_) => replay())),
      li(renderDisablingButton("fa-backward", _ => backStep(), backDisable)),
      li(controlButton),
      li(renderDisablingButton( "fa-forward", _ => nextStep(), nextDisable)),
      li(renderSpeedBar())
    )


  def renderBottomIcon(name: String, icon: String, function: (Any) => Unit): Element =
  button(
    i(
      className := "fa "+icon,
      idAttr := name,

    ),
    onClick --> function

  )

  def renderDisablingButton(icon: String, function: (Any) => Unit, buttonDisabled: Var[Boolean]): Element =
    button(
      i(
        className := "fa " + icon,
        onClick --> function,
      ),

      disabled <-- buttonDisabled
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
    controlButton.ref.innerHTML = stopButton.ref.innerHTML
    dom.document.getElementById("stop").addEventListener("click",  _ => stop())

  def changeStopIcon(): Unit =
    controlButton.ref.innerHTML = playButton.ref.innerHTML
    dom.document.getElementById("play").addEventListener("click", _ => play())

  def enableBackButton(enabled: Boolean): Unit =
    backDisable.set(!enabled)

  def disableNextButton(disable: Boolean): Unit =
    nextDisable.set(disable)

