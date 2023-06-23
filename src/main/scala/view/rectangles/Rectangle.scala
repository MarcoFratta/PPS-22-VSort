package view.rectangles
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.html

//val rectangleWidth = 20
val rectangleHeight = 100
val rectangleCount = 10
def drawRectangle(canvas: html.Canvas): Unit = {
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val rectangleWidth = canvas.width / rectangleCount

  for (i <- 0 until rectangleCount) {
    val x = i * (rectangleWidth + 10) // 10 is the spacing between rectangles
    ctx.fillStyle = "red"
    ctx.fillRect(x, canvas.height - rectangleHeight, rectangleWidth, rectangleHeight)
  }
}

def getRectangle(): Element =
    canvasTag(
      //width := s"${rectangleCount * (rectangleWidth + 10)}",
      height := s"${rectangleHeight}",
      inContext { el =>
      onMountCallback(_ => drawRectangle(el.ref))
     }
  )

