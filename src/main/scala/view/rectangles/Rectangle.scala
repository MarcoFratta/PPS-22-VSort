package view.rectangles
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.html

//val rectangleWidth = 20
val rectangleCount = 10




def drawRectangles(canvas: html.Canvas, list: List[Double]): Unit =
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val rectangleWidth = canvas.width / rectangleCount
  val max = list.max
  drawSingleRectangle(list, 0)
  def drawSingleRectangle(list: List[Double], index: Int): Unit =
    list match
      case h::t =>
        val x = index * (rectangleWidth + 10) // 10 is the spacing between rectangles
          ctx.fillStyle = "red"
          val height = h * canvas.height/ max
          ctx.fillRect(x, canvas.height - height, rectangleWidth, height)
          drawSingleRectangle(t, index+1)
      case Nil =>

def convertToDouble[T](value: T): Double = value match {
  case intValue: Int => intValue.toDouble
  case longValue: Long => longValue.toDouble
  case floatValue: Float => floatValue.toDouble
  case doubleValue: Double => doubleValue
  case stringValue: String => stringValue.toDouble
  case _ => throw new IllegalArgumentException("Value cannot be converted to Double")
}
def convertSeqToDouble[T](seq:Seq[T]): Seq[Double] =
  seq.map(a => convertToDouble(a))
def getRectangle[T](seq: Seq[T]): Element =
    canvasTag(
      //width := s"${rectangleCount * (rectangleWidth + 10)}",
      //height := s"${rectangleHeight}",
      inContext { el =>
      onMountCallback(_ => drawRectangles(el.ref, convertSeqToDouble(seq).toList))
     }
  )

