package model

import com.sun.tools.javac.util.Pair

import scala.annotation.tailrec

object StepsVisualizer {

  import model.Step.*

  def visualize(steps: Seq[Step], array: Seq[Int]): String =
    this.calculate(steps, array.zipWithIndex.map((n, i) => i -> (n.toString, "")).toMap)

  private def calculate(steps: Seq[Step], map: Map[Int, (String, String)]): String = steps match
    case s :: t => getText(s, map)._1 + "\n" + calculate(t, getText(s, map)._2)
    case Nil => "Array finale -> " + map.values.map(v => v._1)

  private def getText(step: Step, map: Map[Int, (String, String)]): (String, Map[Int, (String, String)]) = step match
    case Step.Swap(a, b) => (mapToString(map.updated(a, map(b)).updated(b, map(a))) + " swap",
      map.updated(a, map(b)).updated(b, map(a)))
    case Step.Selection(s, a) => (mapToString(map.updated(a, (map(a)._1, s.toString))) + " selection",
      map.updated(a, (map(a)._1, s.toString)))
    case Step.Comparison(a, b) => (mapToString(map.updated(a, (map(a)._1, "!")).updated(b, (map(b)._1, "!"))) + " comparison",
        map)
    case Step.Deselection(_) => (mapToString(map.map(v => (v._1, (v._2._1, "")))) + " deselection",
      map.map(v => (v._1, (v._2._1, ""))))

  private def mapToString(map: Map[Int, (String, String)]): String =
    map.values.map(p => p._1 + p._2).toString()
}