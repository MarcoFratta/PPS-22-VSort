package controller

import controller.StepController.{SeqProp, SeqProperties, example}
import model.{ElementInfo, InputType}
import view.{GraphFunctions, View, ViewImpl}

trait Controller:
  def getInputList: List[InputType]
  def getElements: Seq[Seq[ElementInfo[Int]]]
  def setSeqSize(size: Int): GraphFunctions
  def addProperties(name: String, value: Int): Map[String, Int]
  def sendData(): GraphFunctions
class ControllerImpl() extends Controller:
  val view: View = new ViewImpl(this)
  private var map: Map[String, Int] = Map()
  override def getInputList: List[InputType] =
    map = Map(("Max", -1), ("Min", -1), ("size", -1))
    List(InputType.SelectList(List("Bubble", "Merge")), InputType.SelectList(List("Normal", "random")),
      InputType.Text("Max"), InputType.Text("Min"), InputType.Slider(10, 100, "size", 50))

  override def getElements: Seq[Seq[ElementInfo[Int]]] =
    println("elements" )
    SeqProperties(map).getElements

  override def setSeqSize(size: Int): GraphFunctions =
    map = map.updated("size", size)
    view.setSeqList(SeqProperties(map).getElements)  


  override def addProperties(name: String, value: Int): Map[String, Int] =
    map = map.updated(name, value)
    println("aggiornata mappa: "+ map)
    map

  override def sendData(): GraphFunctions =
    view.setSeqList(SeqProperties(map).getElements)  