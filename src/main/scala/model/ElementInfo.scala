package model

trait ElementInfo[K]:
  def value: K
  def compared: Boolean
  def selected: Boolean
  def label: Option[String]
  def hidden: Boolean

object ElementInfo:

  def apply[K](value: K,
               compared: Boolean,
               selected: Boolean,
               label: Option[String],
               hidden: Boolean): ElemInfo[K] =
    ElemInfo(value, compared, selected, label, hidden)
  case class ElemInfo[K](override val value: K,
                         override val compared: Boolean,
                         override val selected: Boolean,
                         override val label: Option[String],
                         override val hidden: Boolean) extends ElementInfo[K] {
    override def toString: String = {
      if hidden then
        s"?"
      else
        val comparedString = if compared then "!" else ""
        val labelString = label.getOrElse("")
        s"$value$labelString$comparedString"
    }
  }

  def newInfo[K](value: K): ElemInfo[K] = ElementInfo(value, false, false, Option.empty, false)
  def changeValue(e: ElementInfo[Int], value: Int): ElementInfo[Int] =
    ElementInfo(value, e.compared, e.selected, e.label, e.hidden)
  def select(e: ElementInfo[Int], string: String): ElementInfo[Int] =
    ElementInfo(e.value, e.compared, true, Option(string), e.hidden)
  def deselect(e: ElementInfo[Int]): ElementInfo[Int] =
    ElementInfo(e.value, e.compared, false, Option.empty, e.hidden)
  def compare(e: ElementInfo[Int]): ElementInfo[Int] =
    ElementInfo(e.value, true, e.selected, e.label, e.hidden)
  def show(e: ElementInfo[Int]): ElementInfo[Int] =
    ElementInfo(e.value, e.compared, e.selected, e.label, false)
  def hide(e: ElementInfo[Int]): ElementInfo[Int] =
    ElementInfo(e.value, e.compared, e.selected, e.label, true)
