package model

import scala.annotation.tailrec

object StepsVisualizer {

  import model.Step.*

  @tailrec
  def visualize(steps: Seq[Step], array: Seq[Int]) : Unit = steps match
    case s :: t => val res = getLog(s, array)
      println(res._1.toString + res._3)
      visualize(t, res._2)
    case Nil => print("Array finale -> " + array)

  private def getLog(step: Step, array: Seq[Int]) : (Seq[Int], Seq[Int], String) = step match
    case Step.Swap(a, b) => (array.updated(a, array(b) + 3000).updated(b, array(a) + 3000),
      array.updated(a, array(b)).updated(b, array(a)), " - swap")
    case Step.Selection(_, a) => (array.updated(a, array(a) + 1000), array, " - selected")
    case Step.Comparison(a, b) => (array.updated(a, array(a) + 2000).updated(b, array(b) + 2000), array, " - compared")
    case Step.Deselection(_) => (array, array, " - deselection")
}
