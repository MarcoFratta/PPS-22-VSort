package view.livechart

import com.raquo.laminar.api.L.*

object BottomBar:
  import controller.Graphic.*
  def renderBottomBar() : Element =
    ul(
      li(renderBottomIcon("fa-rotate-left", (_) => ())),
      li(renderBottomIcon("fa-backward", _ => ())),
      li(renderBottomIcon("fa-play", _ => ())),
      li(renderBottomIcon("fa-forward", (_) => nextStep())),
      li(renderBottomIcon("fa-tachometer-alt", _ => ()))
    )

  def renderBottomIcon(icon: String, function: (Any) => Unit): Element =
    button(
      i(
        className := "fa "+icon,
        onClick --> function
      )
    )