package model

import com.sun.tools.javac.util.Pair
import model.ElementInfo.*

import scala.annotation.tailrec

class StepsTransformer[T]:

  import model.Step.*
  private type State = Seq[ElementInfo[T]]

  def getResult(steps: Seq[Step], seq: Seq[T]): Seq[T] = this.applySteps(steps, seq.map(i => newInfo(i)))

  private def applySteps(steps: Seq[Step], seq: State): Seq[T] =
    steps.foldLeft(seq)((currentSeq, step) => getNewSeq(step, currentSeq)).map(e => e.value)

  private def getNewSeq[K](step: Step, seq: State): State = {
    val isComparison = step.isInstanceOf[Comparison[K]]
    if (isComparison) seq else stepFunction(step)(seq)
  }

  private def stepFunction(step: Step): State => State =
    if (step.isInstanceOf[Swap[Int]])
      val swapStep = step.asInstanceOf[Swap[Int]]
      seq =>
        seq.updated(swapStep.a, seq(swapStep.a).changeValue(seq(swapStep.b).value))
          .updated(swapStep.b, seq(swapStep.b).changeValue(seq(swapStep.a).value))
    else if (step.isInstanceOf[Selection[String, Int]])
      val selectionStep = step.asInstanceOf[Selection[String, Int]]
      seq =>
        val deselected = stepFunction(Deselection(selectionStep.s))(seq)
        if (selectionStep.a >= 0 && selectionStep.a < deselected.size)
          deselected.updated(selectionStep.a, deselected(selectionStep.a).select(selectionStep.s))
        else deselected
    else if (step.isInstanceOf[Deselection[String]])
      val deselectionStep = step.asInstanceOf[Deselection[String]]
      seq => seq.map(e => if (e.label == Option(deselectionStep.s)) deselect(e) else e)
    else if (step.isInstanceOf[Comparison[Int]])
      val comparisonStep = step.asInstanceOf[Comparison[Int]]
      seq =>
        seq.updated(comparisonStep.a, compare(seq(comparisonStep.a)))
          .updated(comparisonStep.b, compare(seq(comparisonStep.b)))
    else if (step.isInstanceOf[Divide[Int]])
      val divideStep = step.asInstanceOf[Divide[Int]]
      seq =>
        seq.zipWithIndex.map((e, i) =>
          if (i < divideStep.start || i > divideStep.stop) hide(e) else show(e))
    else
      throw new IllegalArgumentException("Unknown step type")

  def getString(steps: Seq[Step], seq: Seq[T]): String = this.getStepsString(steps, seq.map(i => newInfo(i)))

  private def getStepsString(steps: Seq[Step], map: State): String =
    steps.foldLeft("Array finale -> " + map) ((acc, step) =>
      val seqString = getSeq(step, map).toString
      acc + s" -> $seqString\n" + getNewSeq(step, map))

  private def getSeq(step: Step, map: State): State = stepFunction(step)(map)

  def getSeqList(steps: Seq[Step], seq: Seq[T]): Seq[State] =
    getSeqList(steps, seq.map(i => newInfo(i)), Seq(seq.map(i => newInfo(i))))
      .sliding(2).flatMap{
        case Seq(a, b) if a == b => None
        case Seq(a, _) => Some(a)}.toSeq

  private def getSeqList(steps: Seq[Step], last: State, seq: Seq[State]): Seq[State] =
    steps.foldLeft(Seq(last)) ((acc, step) => acc :+ getNewSeq(step, acc.last)).tail


