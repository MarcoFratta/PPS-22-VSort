package model

import com.sun.tools.javac.util.Pair
import model.ElementInfo.*

import scala.annotation.tailrec

class StepsVisualizer[T]:

  import model.Step.*

  def getResult(steps: Seq[Step], seq: Seq[T]): Seq[T] = this.applySteps(steps, seq.map(i => newInfo(i)))

  @tailrec
  private def applySteps(steps: Seq[Step], seq: Seq[ElementInfo[T]]): Seq[T] = steps match
    case s :: t => applySteps(t, getNewSeq(s, seq))
    case Nil => seq.map(e => e.value)

  private def getNewSeq(step: Step, seq: Seq[ElementInfo[T]]): Seq[ElementInfo[T]] = step match
    case Step.Comparison(_, _) => seq
    case s => stepFunction(s)(seq)

  private def stepFunction(step: Step): Seq[ElementInfo[T]] => Seq[ElementInfo[T]] = step match
    case Step.Swap(a: Int, b: Int) =>
      seq => seq.updated(a, seq(a).changeValue(seq(b).value)).updated(b, seq(b).changeValue(seq(a).value))
    case Step.Selection(s: String, a: Int) => seq => stepFunction(Step.Deselection(s))(seq) match
      case n if a >= 0 && a < n.size => n.updated(a, n(a).select(s))
      case n => n
    case Step.Deselection(s) => seq => seq.map(e => if e.label == Option(s) then deselect(e) else e)
    case Step.Comparison(a: Int, b: Int) => seq => seq.updated(a, compare(seq(a))).updated(b, compare(seq(b)))
    case Step.Divide(a: Int, b: Int) => seq => seq.zipWithIndex.map((e, i) => if i < a || i > b then hide(e) else show(e))

  def getString(steps: Seq[Step], seq: Seq[T]): String = this.getStepsString(steps, seq.map(i => newInfo(i)))

  private def getStepsString(steps: Seq[Step], map: Seq[ElementInfo[T]]): String = steps match
    case s :: t => getSeq(s, map).toString + " -> " + s.toString + "\n" + getStepsString(t, getNewSeq(s, map))
    case Nil => "Array finale -> " + map

  private def getSeq(step: Step, map: Seq[ElementInfo[T]]): Seq[ElementInfo[T]] = stepFunction(step)(map)


  def getSeqList(steps: Seq[Step], seq: Seq[T]): Seq[Seq[ElementInfo[T]]] =
    getSeqList(steps, seq.map(i => newInfo(i)), Seq(seq.map(i => newInfo(i))))
      .sliding(2).flatMap{
        case Seq(a, b) if a == b => None
        case Seq(a, _) => Some(a)}.toSeq

  @tailrec
  private def getSeqList(steps: Seq[Step],
                         last: Seq[ElementInfo[T]],
                         seq: Seq[Seq[ElementInfo[T]]]): Seq[Seq[ElementInfo[T]]] = steps match
    case s :: t => getSeqList(t, getNewSeq(s, last), seq.appended(getSeq(s, last)))
    case Nil => seq