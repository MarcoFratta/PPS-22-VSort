package view.rectangles
import com.raquo.laminar.api.L.{Owner, *}
import model.SeqProperties.Generators.normalDistribution
import org.scalajs.dom
import org.scalajs.dom.html
import model.SeqProperties.Setters.*


def drawRectangles(canvas: html.Canvas, list: List[Int]): Unit =
  val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  ctx.clearRect(0, 0, canvas.width, canvas.height)
  val rectangleWidth = canvas.width / (1.5*list.size)
  val max = list.max
  drawSingleRectangle(list, 0)
  def drawSingleRectangle(list: List[Int], index: Int): Unit =
    list match
      case h::t =>
        val x = index * (rectangleWidth +0.5) // 10 is the spacing between rectangles
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
def computeAndDraw(canvas: dom.html.Canvas, seq:List[Int], size: Int): Unit =
  println("sizeComputeAndDraw :" +size)
  //val seq = normalDistribution(50,  15).take(size).shift(1, 200).doubleToInt

  drawRectangles(canvas, seq)
  // Esegui le operazioni desiderate sul canvas utilizzando il valore fornito
  // ...

def getRectangle[T](seq: List[Int], size: Var[Int]): Element =

  canvasTag(
    className := "canvas",
    //width := s"${rectangleCount * (rectangleWidth + 10)}",
    //height := s"${rectangleHeight}",
    //size.signal--> (newValue => computeAndDraw(re , newValue)),
    inContext(thisNode => size.signal--> (newValue => computeAndDraw(thisNode.ref,seq, newValue))),
    onMountCallback(el =>
      size.signal --> (newValue => computeAndDraw(el.thisNode.ref, seq, newValue))
      computeAndDraw(el.thisNode.ref, seq, size.now())
    )
  )

def getAllSteps[T](list: List[List[Int]]): Element =
  var i =0
  canvasTag(
    className := "canvas",
    //width := s"${rectangleCount * (rectangleWidth + 10)}",
    //height := s"${rectangleHeight}",
    //size.signal--> (newValue => computeAndDraw(re , newValue)),
    //inContext(thisNode => size.signal--> (newValue => computeAndDraw(thisNode.ref,seq, newValue))),
    onMountCallback(el =>
      val timerIdRef: Var[Option[Int]] = Var(None)
      def updateCanvas(): Unit=
        computeAndDraw(el.thisNode.ref, list(i), list(i).size)
        i = i+1
        if i equals(list.size) then
          timerIdRef.now().foreach(dom.window.clearInterval)
          timerIdRef.set(None)



      val timerId = dom.window.setInterval(() => updateCanvas(), 10)
      timerIdRef.set(Some(timerId))
      /*onUnmountCallback { _ =>
        dom.window.clearInterval(timerId)
      }
      */

    )
  )
