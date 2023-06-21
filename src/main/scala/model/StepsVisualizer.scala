package model

import com.sun.tools.javac.util.Pair

import scala.annotation.tailrec

class Entry(val value: String, val label: String) {
  override def toString: String = s"($value, $label)"
}

object StepsVisualizer {

  import model.Step.*

  def visualizeSteps(steps: Seq[Step], array: Seq[Int]): String =
    this.applySteps(steps, arrayToMap(array))

  private def applySteps(steps: Seq[Step], map: Map[Int, Entry]): String = steps match
    case s :: t => getText(s, map) + "\n" + applySteps(t, getNewMap(s, map))
    case Nil => "Array finale -> " + mapToString(map)

  private def getText(step: Step, map: Map[Int, Entry]): String =
    mapToString(stepFunction(step)(map)) + " -> " + step.toString
  private def getNewMap(step: Step, map: Map[Int, Entry]): Map[Int, Entry] = step match
    case Step.Comparison(_, _) => map
    case s => stepFunction(s)(map)

  private def stepFunction(step: Step): Map[Int, Entry] => Map[Int, Entry] = step match
    case Step.Swap(a: Int, b: Int) => map => map.updated(a, map(b)).updated(b, map(a))
    case Step.Selection(s, a: Int) => map => map.updated(a, Entry(map(a).value, s.toString))
    case Step.Comparison(a: Int, b: Int) => map => map.updated(a, Entry(map(a).value, "!"))
      .updated(b, Entry(map(b).value, "!"))
    case Step.Deselection(s) => map => map.mapValues(e => if (e.label == s) new Entry(e.value, "") else e).toMap

  private def arrayToMap(array: Seq[Int]): Map[Int, Entry] =
    array.zipWithIndex.map((n, i) => i -> Entry(n.toString, "")).toMap

  private def mapToString(map: Map[Int, Entry]): String =
    "[" + map.toList.sortBy(_._1).map((_, p) => p.value + p.label).mkString(", ") + "]"
}