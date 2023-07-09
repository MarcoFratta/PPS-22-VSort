package model

import com.sun.tools.javac.util.Pair

import scala.annotation.tailrec

class Entry(val value: String, val label: String) {
  override def toString: String = s"($value, $label)"
}

object StepsVisualizer {

  import model.Step.*

  // data una sequenza di step e l'array iniziale, ritorna l'array finale
  def getResult(steps: Seq[Step], array: Seq[Int]): Seq[Int] = this.applySteps(steps, arrayToMap(array))

  // trasforma l'array in una mappa indice -> valore, label
  private def arrayToMap(array: Seq[Int]): Map[Int, Entry] =
    array.zipWithIndex.map((n, i) => i -> Entry(n.toString, "")).toMap

  // applica alla mappa tutti gli step della lista
  @tailrec
  private def applySteps(steps: Seq[Step], map: Map[Int, Entry]): Seq[Int] = steps match
    case s :: t => applySteps(t, getNewMap(s, map))
    case Nil => map.toList.sortBy(_._1).map((_, p) => p.value.toInt)

  // applica uno step alla mappa
  private def getNewMap(step: Step, map: Map[Int, Entry]): Map[Int, Entry] = step match
    case Step.Comparison(_, _) => map
    case s => stepFunction(s)(map)

  // dato uno step, ritorna la funzione da applicare alla mappa
  private def stepFunction(step: Step): Map[Int, Entry] => Map[Int, Entry] = step match
    case Step.Swap(a: Int, b: Int) => map
      => map.updated(a, Entry(map(b).value, map(a).label))
        .updated(b, Entry(map(a).value, map(b).label))
    case Step.Selection(s, a: Int) => map => if a < map.size
      then
        stepFunction(Step.Deselection(s))(map).updated(a, Entry(map(a).value, s.toString))
      else
        stepFunction(Step.Deselection(s))(map)
    case Step.Comparison(a: Int, b: Int) => map
      => map.updated(a, Entry(map(a).value, map(a).label + "!"))
        .updated(b, Entry(map(b).value, map(b).label + "!"))
    case Step.Deselection(s) => map
      => map.mapValues(e => if (e.label == s) new Entry(e.value, "") else e).toMap
    case Step.Divide(a: Int, b: Int) => map
      => stepFunction(Step.Deselection("X"))(map)
        .map((k, v) => if k < a || k > b then (k, Entry(v.value, v.label + "X")) else (k, v))



  // data una sequenza di step e l'array iniziale, ritorna una stringa con tutti i passaggi
  def getString(steps: Seq[Step], array: Seq[Int]): String = this.getStepsString(steps, arrayToMap(array))

  private def getStepsString(steps: Seq[Step], map: Map[Int, Entry]): String = steps match
    case s :: t => mapToString(getMap(s, map)) + " -> " + s.toString + "\n" + getStepsString(t, getNewMap(s, map))
    case Nil => "Array finale -> " + mapToString(map)

  // dato uno step e una mappa, applica lo step e stampa il risulato
  private def getMap(step: Step, map: Map[Int, Entry]): Map[Int, Entry] = stepFunction(step)(map)




  // data una sequenza di step e l'array iniziale, ritorna una lista di tutti gli array ad ogni step
  def getMapList(steps: Seq[Step], array: Seq[Int]): List[List[(Int, String)]] =
    getMapList(steps, arrayToMap(array), List(arrayToMap(array))).map(m => m.toList.sortBy(_._1).map((_, p) => (p.value.toInt, p.label)))

  @tailrec
  private def getMapList(steps: Seq[Step], last: Map[Int, Entry], maps: List[Map[Int, Entry]]): List[Map[Int, Entry]] = steps match
    case s :: t => getMapList(t, getNewMap(s, last), maps.appended(getMap(s, last)))
    case Nil => maps













  private def mapToString(map: Map[Int, Entry]): String =
    "[" + map.toList.sortBy(_._1).map((_, p) => p.value + p.label).mkString(", ") + "]"
}