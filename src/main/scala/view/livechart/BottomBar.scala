package view.livechart

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import view.livechart.BottomBar.renderBottomIcon
object BottomBar:
  import controller.Graphic.*

  var playButton = renderBottomIcon("play", "fa-play", _ => play())
  var stopButton = renderBottomIcon("stop", "fa-stop", _ => stop())
  var controlButton = renderBottomIcon("", "fa-play", _ => play())

  def renderBottomBar() : Element =

    ul(
      li(renderBottomIcon("", "fa-rotate-left", (_) => ())),
      li(renderBottomIcon("", "fa-backward", _ => backStep())),
      li(controlButton),
      li(renderBottomIcon("", "fa-forward", _ => nextStep())),
      li(renderBottomIcon("", "fa-tachometer-alt", _ => ()))
    )


  def renderBottomIcon(name: String, icon: String, function: (Any) => Unit): Element =
  button(
    i(
      idAttr := name,
      className := "fa "+icon,
      onClick --> function
    )
  )
  def changePlayIcon(): Unit =
    controlButton.ref.innerHTML = stopButton.ref.innerHTML
    dom.document.getElementById("stop").addEventListener("click",  _ => stop())

  def changeStopIcon(): Unit =
    //dom.document.getElementById("stop").replaceWith(playButton.ref)
    println("change stop icon")
    controlButton.ref.innerHTML = playButton.ref.innerHTML
    dom.document.getElementById("play").addEventListener("click", _ => play())
