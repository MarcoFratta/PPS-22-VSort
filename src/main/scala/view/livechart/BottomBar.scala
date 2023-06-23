package view.livechart

import com.raquo.laminar.api.L.*

object BottomBar:

  def renderBottomBar() : Element =
    ul(
      li(renderBottomIcon("fa-rotate-left")),
      li(renderBottomIcon("fa-backward")),
      li(renderBottomIcon("fa-play")),
      li(renderBottomIcon("fa-forward")),
      li(renderBottomIcon("fa-tachometer-alt"))
    )

  def renderBottomIcon(icon: String): Element =
    button(
      i(
        className := "fa "+icon
      )
    )