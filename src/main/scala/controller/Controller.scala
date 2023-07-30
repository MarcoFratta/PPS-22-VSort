package controller

import controller.StepController.{SeqProp, SeqProperties, example}
import model.{Algorithm, Distribution, ElementInfo, HasName, InputType, IntModelImpl, Model, Params, State}
import view.{GraphFunctions, View, ViewImpl}
trait Controller:
  //def getInputList: List[InputType]
  //def getVariableInputList: List[InputType]
  //def getElements: Seq[Seq[ElementInfo[Int]]]
  //def setSeqSize(size: Int): GraphFunctions
  //def addProperties(name: String, value: Int): Properties
  def setProperties(): View
  def getAlgorithms: Set[Algorithm[Int, Seq[State[ElementInfo[Int]]]] with HasName ]
  def getDistribution: Set[Distribution[Int, Int] with HasName]
  def getSeqElement: Seq[Seq[ElementInfo[Int]]]
  def getProperties: Properties
  //def sendData(): GraphFunctions
  //def onOptionSelected(field: String, value: String): Unit



trait Properties:
  def alg: Algorithm[Int, Seq[State[ElementInfo[Int]]]] with HasName
  def alg_=(algorithm: Algorithm[Int, Seq[State[ElementInfo[Int]]]] with HasName): Unit
  def distribution: Distribution[Int, Int] with HasName
  def distribution_=(distribution: Distribution[Int, Int] with HasName): Unit
  def paramMap: Map[Params, Int]
  def paramMap_=(map: Map[Params, Int]): Unit
case class PropertiesImpl(override var alg: Algorithm[Int, Seq[State[ElementInfo[Int]]]] with HasName,
                      override var distribution: Distribution[Int, Int] with HasName,
                      override var paramMap: Map[Params, Int]) extends Properties
class ControllerImpl(prop: PropertiesImpl) extends Controller:
  println("nuovo controller")
  println(prop)
  val view: View = new ViewImpl(this)
  //val topBar = TopBar(this)

  override def getAlgorithms: Set[Algorithm[Int, Seq[State[ElementInfo[Int]]]] with HasName]= IntModelImpl().algorithms

  override def getDistribution: Set[Distribution[Int, Int] with HasName] = IntModelImpl().distributions


  override def getProperties: Properties = prop
  def getDistributionParams(dis: Distribution[Int, Int] with HasName): Map[Params, Int] =
    IntModelImpl().distributions.find(a => a.name equals dis.name).get.params.map(a => a -> -1).toMap
    //IntModelImpl().distributions.find(a => a == dis).get.params. groupMapReduce(a => a)(a => -1)((b,b) => b)
  override def setProperties(): View =
    //if prop.distribution != null then
     // renderElement.renderParamsTopBar(getDistributionParams(prop.distribution))
    if checkField(prop)
      then new ViewImpl(this)
    else view
 /* override def getElements: Seq[Seq[ElementInfo[Int]]] =
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


  */
  def getSeqElement: Seq[Seq[ElementInfo[Int]]] =
    SeqProperties(prop).getElements
/*
      case "Algorithm" => properties.alg = IntModelImpl().algorithms.filter(a => a.name eq(value)).toList.head
      case "Distribution" =>
        properties.dioverride def onOptionSelected(field: String, value: String): Unit =
  field match
stribution = IntModelImpl().distributions.filter(a => a.name eq(value)).toList.head
        properties.map = defaultMap
        properties.distribution.params.foreach(a => properties.map = properties.map.updated(a, -1))
        println("distribution")
        properties.distribution.params.foreach(a => println(a.toString))
        properties.map.foreach(a => println(a._1.toString))
        TopBar(this).replaceTopBar(computeVariableInputList(properties.distribution))
      case _ =>
    if checkField() then sendData()

*/

  private def checkField(properties: Properties): Boolean =
    properties.alg != null && properties.distribution != null && properties.paramMap.nonEmpty &&
      properties.paramMap.filter(a => a._2 < 0).toList.isEmpty