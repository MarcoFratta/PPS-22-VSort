package view.livechart

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom
import view.livechart.BottomBar.renderBottomIcon
object BottomBar:
  import controller.Graphic.*

  var playButton = renderBottomIcon("play", "fa-play", _ => play())
  var stopButton = renderBottomIcon("stop", "fa-stop", _ => println("Clickkk"))


  def renderBottomBar() : Element =

    ul(
      li(renderBottomIcon("", "fa-rotate-left", (_) => ())),
      li(renderBottomIcon("", "fa-backward", _ => ())),
      li(playButton),
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
    playButton.ref.innerHTML = stopButton.ref.innerHTML
    dom.document.getElementById("stop").addEventListener("click",  _ => stop())


