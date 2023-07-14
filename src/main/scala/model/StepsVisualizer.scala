package model

import com.sun.tools.javac.util.Pair
import model.ElementInfo.*

import scala.annotation.tailrec

object StepsVisualizer:

  import model.Step.*

  def getResult(steps: Seq[Step], seq: Seq[Int]): Seq[Int] = this.applySteps(steps, seq.map(i => newInfo(i)))

  @tailrec
  private def applySteps(steps: Seq[Step], seq: Seq[ElementInfo[Int]]): Seq[Int] = steps match
    case s :: t => applySteps(t, getNewSeq(s, seq))
    case Nil => seq.map(e => e.value)

  private def getNewSeq(step: Step, seq: Seq[ElementInfo[Int]]): Seq[ElementInfo[Int]] = step match
    case Step.Comparison(_, _) => seq
    case s => stepFunction(s)(seq)

  private def stepFunction(step: Step): Seq[ElementInfo[Int]] => Seq[ElementInfo[Int]] = step match
    case Step.Swap(a: Int, b: Int) =>
      seq => seq.updated(a, changeValue(seq(a), seq(b).value)).updated(b, changeValue(seq(b), seq(a).value))
    case Step.Selection(s: String, a: Int) => seq => stepFunction(Step.Deselection(s))(seq) match
      case n if a < n.size => n.updated(a, select(n(a), s))
      case n => n
    case Step.Deselection(s) => seq => seq.map(e => if e.label == Option(s) then deselect(e) else e)
    case Step.Comparison(a: Int, b: Int) => seq => seq.updated(a, compare(seq(a))).updated(b, compare(seq(b)))
    case Step.Divide(a: Int, b: Int) => seq => seq.map(e => if seq.indexOf(e) < a || seq.indexOf(e) > b then hide(e) else show(e))

  def getString(steps: Seq[Step], seq: Seq[Int]): String = this.getStepsString(steps, seq.map(i => newInfo(i)))

  private def getStepsString(steps: Seq[Step], map: Seq[ElementInfo[Int]]): String = steps match
    case s :: t => getSeq(s, map).toString + " -> " + s.toString + "\n" + getStepsString(t, getNewSeq(s, map))
    case Nil => "Array finale -> " + map

  private def getSeq(step: Step, map: Seq[ElementInfo[Int]]): Seq[ElementInfo[Int]] = stepFunction(step)(map)


  def getSeqList(steps: Seq[Step], seq: Seq[Int]): Seq[Seq[ElementInfo[Int]]] =
    getSeqList(steps, seq.map(i => newInfo(i)), Seq(seq.map(i => newInfo(i))))

  @tailrec
  private def getSeqList(steps: Seq[Step],
                         last: Seq[ElementInfo[Int]],
                         seq: Seq[Seq[ElementInfo[Int]]]): Seq[Seq[ElementInfo[Int]]] = steps match
    case s :: t => getSeqList(t, getNewSeq(s, last), seq.appended(getSeq(s, last)))
    case Nil => seq