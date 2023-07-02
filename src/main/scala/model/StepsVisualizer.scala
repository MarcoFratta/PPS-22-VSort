package model

import com.sun.tools.javac.util.Pair

import scala.annotation.tailrec

class Entry(val value: String, val label: String) {
  override def toString: String = s"($value, $label)"
}

object StepsVisualizer {

  import model.Step.*

  def getSteps(steps: Seq[Step], array: Seq[Int]): (Seq[Int], String) =
    (this.applySteps(steps, arrayToMap(array)), this.getStepsString(steps, arrayToMap(array)))

  def getMapList(steps: Seq[Step], array: Seq[Int]): List[List[Int]] =
    getMapList(steps, List(arrayToMap(array))).map(m => m.toList.sortBy(_._1).map((_, p) => p.value.toInt))

  @tailrec
  private def getMapList(steps: Seq[Step], maps: List[Map[Int, Entry]]): List[Map[Int, Entry]] = steps match
    case s :: t => getMapList(t, maps.appended(getNewMap(s, maps.last)))
    case Nil => maps

  @tailrec
  private def applySteps(steps: Seq[Step], map: Map[Int, Entry]): Seq[Int] = steps match
    case s :: t => applySteps(t, getNewMap(s, map))
    case Nil => map.toList.sortBy(_._1).map((_, p) => p.value.toInt)

  private def getStepsString(steps: Seq[Step], map: Map[Int, Entry]): String = steps match
    case s :: t => getText(s, map) + "\n" + getStepsString(t, getNewMap(s, map))
    case Nil => "Array finale -> " + mapToString(map)

  private def getText(step: Step, map: Map[Int, Entry]): String =
    mapToString(stepFunction(step)(map)) + " -> " + step.toString
  private def getNewMap(step: Step, map: Map[Int, Entry]): Map[Int, Entry] = step match
    case Step.Comparison(_, _) => map
    case Step.Divide(_, _) => map
    case s => stepFunction(s)(map)

  private def stepFunction(step: Step): Map[Int, Entry] => Map[Int, Entry] = step match
    case Step.Swap(a: Int, b: Int) => map => map.updated(a, Entry(map(b).value, map(a).label))
      .updated(b, Entry(map(a).value, map(b).label))
    case Step.Selection(s, a: Int) => map => if a < map.size
      then
        stepFunction(Step.Deselection(s))(map).updated(a, Entry(stepFunction(Step.Deselection(s))(map)(a).value, s.toString))
      else
        stepFunction(Step.Deselection(s))(map)
    case Step.Comparison(a: Int, b: Int) => map => map.updated(a, Entry(map(a).value, map(a).label + "!"))
      .updated(b, Entry(map(b).value, map(b).label + "!"))
    case Step.Deselection(s) => map => map.mapValues(e => if (e.label == s) new Entry(e.value, "") else e).toMap
    case Step.Divide(a: Int, b: Int) => map => map.filter((k, _) => k >= a && k <= b)

  private def arrayToMap(array: Seq[Int]): Map[Int, Entry] =
    array.zipWithIndex.map((n, i) => i -> Entry(n.toString, "")).toMap

  private def mapToString(map: Map[Int, Entry]): String =
    "[" + map.toList.sortBy(_._1).map((_, p) => p.value + p.label).mkString(", ") + "]"
}