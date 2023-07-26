package controller

import controller.StepController.{SeqProp, SeqProperties, example}
import model.{Algorithm, Distribution, ElementInfo, InputType, Model, ModelImpl, State}
import view.{GraphFunctions, View, ViewImpl}
trait Controller:
  def getInputList: List[InputType]
  def getElements: Seq[Seq[ElementInfo[Int]]]
  def setSeqSize(size: Int): GraphFunctions
  def addProperties(name: String, value: Int): Map[String, Int]
  def sendData(): GraphFunctions
  def setAlghorithm(alg: String): Unit
  def setDistribution(dis: String): Unit


trait Properties:
  def alg: Algorithm[Int, Seq[State[ElementInfo[Int]]]]
  def alg_=(algorithm: Algorithm[Int, Seq[State[ElementInfo[Int]]]]): Unit
  def distribution: Distribution[Int, Int]
  def distribution_=(distribution: Distribution[Int, Int]): Unit
  def map: Map[String, Int]
  def map_=(map: Map[String, Int]): Unit
case class PropertiesImpl(override var alg: Algorithm[Int, Seq[State[ElementInfo[Int]]]],
                      override var distribution: Distribution[Int, Int],
                      override var map: Map[String, Int]) extends Properties
class ControllerImpl() extends Controller:
  val view: View = new ViewImpl(this)
  val properties = PropertiesImpl(ModelImpl().algorithms.toList.head,
    ModelImpl().distributions.toList.head, Map())
  override def getInputList: List[InputType] =
    val defaultSize = 50
    properties.map = Map(("max", -1), ("min", -1), ("size", defaultSize))

    List(InputType.SelectList(ModelImpl().algorithms.map(a => a.name).toList),
      InputType.SelectList(ModelImpl().distributions.map(a => a.name).toList),
      InputType.Text("max"), InputType.Text("min"), InputType.Text("% valori duplicati"),
      InputType.Slider(10, 100, "size", defaultSize))

  override def getElements: Seq[Seq[ElementInfo[Int]]] =
    println("elements" )
    SeqProperties(properties).getElements

  override def setSeqSize(size: Int): GraphFunctions =
    properties.map = properties.map.updated("size", size)
    view.setSeqList(SeqProperties(properties).getElements)


  override def addProperties(name: String, value: Int): Map[String, Int] =
    properties.map = properties.map.updated(name, value)
    println("aggiornata mappa: "+ properties.map)
    if checkField() then sendData()
    properties.map



  override def sendData(): GraphFunctions =
    view.setSeqList(SeqProperties(properties).getElements)

  override def setAlghorithm(algName: String): Unit =
    println("settato" + algName)
    properties.alg = ModelImpl().algorithms.filter(a => a.name eq(algName)).toList.head
    if checkField() then sendData()

  override def setDistribution(dis: String): Unit =
    println("settato" + dis)

    properties.distribution = ModelImpl().distributions.filter(a => a.name == dis).toList.head
    if checkField() then sendData()


  private def checkField(): Boolean =
    properties.alg != null && properties.distribution != null &&
      properties.map.filter(a => a._2 < 0).toList.isEmpty