//package view
//
//import com.raquo.laminar.api.L.{Element, button, canvasTag, child, children, className, div, i, li, nodeSeqToModifier, onClick, onLoad, onMountBind, onMountCallback, onMountInsert, render, renderOnDomContentLoaded, ul, windowEvents}
//import com.raquo.laminar.api.eventPropToProcessor
//import controller.{Controller, Properties}
//import model.{Algorithm, Distribution, ElementInfo, HasName, ModelTypes, Params, State}
//import model.InputType.*
//import org.scalajs.dom
//
//trait View extends ModelTypes:
//  def getAppElement: Element
//  def updated(data:ResultType): View
//
//case class ViewImpl(data:Seq[State[ElementInfo[Int]]],
//                    algorithms:Set[Algorithm[Int,Seq[State[ElementInfo[Int]]]] with HasName],
//                    distributions:Set[Distribution[Int,Int] with HasName],
//                    p:Properties[Int,Int,Seq[State[ElementInfo[Int]]]]) extends View:
//
//  import BottomBar.*
//  private val algo = MultipleListFactory(algorithms, p.algorithm)
//  private val dis = MultipleListFactory(distributions, p.distribution)
//  val params = p.params.map(a => SingleValue(1,1))
//  //replaceView
//  //setSeqList()
//  //dom.window.onload = _ => setSeqList()
//  dom.document.getElementById("app").innerHTML = ""
//  render(
//    dom.document.getElementById("app"),
//    getAppElement
//  )
//  replaceView()
//  //windowEvents(_.onLoad).foreach {_ => setSeqList()}(unsafeWindowOwner)
//
//  private def replaceView(): Unit =
//    println("replacing view")
//    //dom.document.getElementById("app").innerHTML = ""
//    //render(dom.document.getElementById("app"), getAppElement)
//
//    dom.document.getElementById("app").addEventListener("onChange", _ =>  updated())
//
//
//
//  override def getAppElement: Element =
//    println("getappelem")
//    div(
//        div(
//
//          ul(className := "topBar",
//            li(algo.element),
//            li(dis.element),
//            params.map(a => li(a.element)),
//            /*
//            div(
//              onMountBind { divNode =>
//                // Aggiungi gli elementi li all'elemento div durante il montaggio
//                params.map { item =>
//                  divNode.thisNode.ref.appendChild(li(item.element).ref)
//                }
//              }
//            ),
//            */
//            li(
//              button(
//                i(
//                  className := "fa fa-check",
//                  onClick --> (_ =>
//                    println("click")
////                    new ControllerImpl(new PropertiesImpl(alg = algo.get.head._1, distribution = dis.get.head._1,
////                      Params = params.map(a => a.get).toList.foldLeft(Map.empty[Params, Int]) { (acc, map) =>
////                        acc ++ map
////                      } ))
//
//                    )
//                )
//              )
//            )
//            //div(params.map(a => a.element).map(a => li(a)))
//
//          //AlgTopBar(controller).getElements,
//          //RenderElements(this).ulElement
//          //DistributionTopBar(controller).getElements,
//          )
//        ),
//        onMountCallback (_ => updated()),
//        div(canvasTag(
//          className := "canvas")),
//        div(className:= "bottomBar"),
//       // BottomBar(controller).renderBottomBar()
//
//    )
//
//  def computeUl: Element =
//    div(params.map(a => li(a.element)))
//
//  override def updated(): GraphFunctions = ???
//    //println("setSeqList")
//    //GraphFunctions(controller.getData)
//
//  //private def findParamFromName(name: String): Params =
//  //  properties.map.filter(a => a._1.toString equals name).toList.head._1
//
//
