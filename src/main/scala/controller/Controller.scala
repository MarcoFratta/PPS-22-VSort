package controller

import controller.StepController.{SeqProp, SeqProperties, example}
import model.{Algorithm, Distribution, ElementInfo, InputType, IntModelImpl, Model, Params, State}
import view.{GraphFunctions, TopBar, View, ViewImpl}
trait Controller:
  def getInputList: List[InputType]
  def getVariableInputList: List[InputType]
  def getElements: Seq[Seq[ElementInfo[Int]]]
  def setSeqSize(size: Int): GraphFunctions
  def addProperties(name: String, value: Int): Map[Params, Int]
  def sendData(): GraphFunctions
  def onOptionSelected(field: String, value: String): Unit


trait Properties:
  def alg: Algorithm[Int, Seq[State[ElementInfo[Int]]]]
  def alg_=(algorithm: Algorithm[Int, Seq[State[ElementInfo[Int]]]]): Unit
  def distribution: Distribution[Int, Int]
  def distribution_=(distribution: Distribution[Int, Int]): Unit
  def map: Map[Params, Int]
  def map_=(map: Map[Params, Int]): Unit
case class PropertiesImpl(override var alg: Algorithm[Int, Seq[State[ElementInfo[Int]]]],
                      override var distribution: Distribution[Int, Int],
                      override var map: Map[Params, Int]) extends Properties
class ControllerImpl() extends Controller:
  val view: View = new ViewImpl(this)
  val properties = PropertiesImpl(IntModelImpl().algorithms.toList.head,
    IntModelImpl().distributions.toList.head, Map())

  override def getInputList: List[InputType] =
    val defaultSize = 50
    //properties.map = Map(("max", -1), ("min", -1), ("size", defaultSize))
    var list: List[InputType] = List(InputType.SelectList("Algorithm", IntModelImpl().algorithms.map(a => a.name).toList),
      InputType.SelectList("Distribution", IntModelImpl().distributions.map(a => a.name).toList))
    list
    //computeInputList(properties.distribution)
      //InputType.Text("max"), InputType.Text("min"), InputType.Text("% valori duplicati"),
      //InputType.Slider(10, 100, "size", defaultSize))
  private def computeVariableInputList(distribution: Distribution[Int, Int]): List[InputType] =
      var list: List[InputType] = List()
      
      properties.distribution.params.foreach(a => properties.map = properties.map.updated(a, -1))

      list = list.concat(distribution.params.map(a => InputType.Text(a.toString)).toList)
      //list = list.appended(InputType.Slider(10, 100, "Size", 50))
      list

  override def getVariableInputList: List[InputType] =
    computeVariableInputList(properties.distribution)  
  override def getElements: Seq[Seq[ElementInfo[Int]]] =
    println("elements" )
    SeqProperties(properties).getElements

  override def setSeqSize(size: Int): GraphFunctions =
    properties.map = properties.map.updated(findParamFromName("Size"), size)
    view.setSeqList(SeqProperties(properties).getElements)


  override def addProperties(name: String, value: Int): Map[Params, Int] =
    properties.map = properties.map.updated(findParamFromName(name), value)
    println("aggiornata mappa: "+ properties.map)
    if checkField() then sendData()
    properties.map

  private def findParamFromName(name: String): Params =
    properties.map.filter(a => a._1.toString equals name).toList.head._1

  override def sendData(): GraphFunctions =
    view.setSeqList(SeqProperties(properties).getElements)

  override def onOptionSelected(field: String, value: String): Unit =
    field match
      case "Algorithm" => properties.alg = IntModelImpl().algorithms.filter(a => a.name eq(value)).toList.head
      case "Distribution" =>
        properties.distribution = IntModelImpl().distributions.filter(a => a.name eq(value)).toList.head
        properties.map = Map()
        properties.distribution.params.foreach(a => properties.map = properties.map.updated(a, -1))
        println("distribution")
        properties.distribution.params.foreach(a => println(a.toString))
        properties.map.foreach(a => println(a._1.toString))
        TopBar(this).replaceTopBar(computeVariableInputList(properties.distribution))
      case _ =>
    if checkField() then sendData()


  private def checkField(): Boolean =
    properties.alg != null && properties.distribution != null && properties.map.nonEmpty &&
      properties.map.filter(a => a._2 < 0).toList.isEmpty