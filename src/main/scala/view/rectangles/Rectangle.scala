package view.rectangles
import com.raquo.laminar.api.L.{Owner, *}
import model.SeqProperties.Generators.normalDistribution
import org.scalajs.dom
import org.scalajs.dom.html
import model.SeqProperties.Setters.*



def drawRectangles(canvas: html.Canvas, list: List[Double]): Unit =
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  ctx.clearRect(0, 0, canvas.width, canvas.height)
  val rectangleWidth = canvas.width / list.size
  val max = list.max
  drawSingleRectangle(list, 0)
  def drawSingleRectangle(list: List[Double], index: Int): Unit =
    list match
      case h::t =>
        val x = index * (rectangleWidth +1) // 10 is the spacing between rectangles
          ctx.fillStyle = "red"
          val height = (h * canvas.height) / max
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
def convertSeqToDouble[T](seq:Seq[T])(using f: T => Double): Seq[Double] =
  seq.map(a => f(a))
def computeAndDraw(canvas: dom.html.Canvas, size: Int): Unit =
  println("sizeComputeAndDraw :" +size)
  val seq = normalDistribution(50,  15).take(size).shift(1, 200).doubleToInt

  drawRectangles(canvas, seq.map(a => a.toDouble).toList)
  // Esegui le operazioni desiderate sul canvas utilizzando il valore fornito
  // ...

def getRectangle[T](size: Var[Int]): Element =

  canvasTag(
    className := "canvas",
    //width := s"${rectangleCount * (rectangleWidth + 10)}",
    //height := s"${rectangleHeight}",
    //size.signal--> (newValue => computeAndDraw(re , newValue)),
    inContext(thisNode => size.signal--> (newValue => computeAndDraw(thisNode.ref , newValue))),
    onMountCallback(el =>
      size.signal --> (newValue => computeAndDraw(el.thisNode.ref, newValue))
      computeAndDraw(el.thisNode.ref, size.now())
    )
  )

