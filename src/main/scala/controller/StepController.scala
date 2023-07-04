package controller
import com.raquo.laminar.api.L.Var
import model.*
import model.SeqProperties.Generators.uniformDistribution

case class UniformDistribution(min: Int, max:Int, size:Int)

object Graphic:
  import view.rectangles.*
  def showGraph(): Unit =
    val seq = uniformDistribution(0,100).take(50).map(a => a.toInt)
    drawGraphic(seq.toList, Var(seq.size))
