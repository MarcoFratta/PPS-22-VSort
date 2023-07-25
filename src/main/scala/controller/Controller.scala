package controller

import controller.StepController.{SeqProp, SeqProperties, example}
import model.{ElementInfo, InputType}
import view.{GraphFunctions, View, ViewImpl}

trait Controller:
  def getInputList: List[InputType]
  def getElements: Seq[Seq[ElementInfo[Int]]]
  def setSeqSize(size: Int): GraphFunctions
  def setSeqList(): GraphFunctions
  def addProperties(name: String, value: Int): Map[String, Int]
class ControllerImpl() extends Controller:
  val view: View = new ViewImpl(this)
  private val seqProp: SeqProp = SeqProperties(view)
  private var map: Map[String, Int] = Map()
  override def getInputList: List[InputType] =
    map = Map(("Max", -1), ("Min", -1), ("size", -1))
    List(InputType.SelectList(List("Bubble", "Merge")), InputType.SelectList(List("Normal", "random")),
      InputType.Text("Max"), InputType.Text("Min"), InputType.Slider(10, 100, "size", 50))

  override def getElements: Seq[Seq[ElementInfo[Int]]] =
    println("elements" )
    seqProp.getElements

  override def setSeqSize(size: Int): GraphFunctions = seqProp.setSize(size)

  override def setSeqList(): GraphFunctions = seqProp.setSeqList()

  override def addProperties(name: String, value: Int): Map[String, Int] =
    map = map.updated(name, value)
    println("aggiornata mappa: "+ map)
    map