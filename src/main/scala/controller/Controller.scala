package controller

import controller.StepController.{SeqProp, SeqProperties, example}
import model.{ElementInfo, InputType}
import view.{GraphFunctions, View, ViewImpl}

trait Controller:
  def getInputList: List[InputType]
  def getElements: Seq[Seq[ElementInfo[Int]]]
  def setSeqSize(size: Int): GraphFunctions
  def setSeqList(): GraphFunctions
class ControllerImpl() extends Controller:
  val view: View = new ViewImpl(this)
  private val seqProp: SeqProp = SeqProperties(view)
  override def getInputList: List[InputType] =
    List(InputType.SelectList(List("Bubble", "Merge")), InputType.SelectList(List("Normal", "random")),
      InputType.Text("Max"), InputType.Text("Min"), InputType.Slider(10, 100, "size", 50))

  override def getElements: Seq[Seq[ElementInfo[Int]]] =
    println("elements" )
    seqProp.getElements

  override def setSeqSize(size: Int): GraphFunctions = seqProp.setSize(size)

  override def setSeqList(): GraphFunctions = seqProp.setSeqList()
