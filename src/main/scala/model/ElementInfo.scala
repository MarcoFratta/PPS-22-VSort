package model

trait ElementInfo[K]:
  def value: K
  def compared: Boolean
  def selected: Boolean
  def label: Option[String]
  def hidden: Boolean

object ElementInfo:
  extension [K](e: ElementInfo[K])
    def changeValue(value: K): ElementInfo[K] =
      ElementInfo(value, e.compared, e.selected, e.label, e.hidden)
    def select(string: String): ElementInfo[K] =
      ElementInfo(e.value, e.compared, true, Option(string), e.hidden)
    def deselect: ElementInfo[K] =
      ElementInfo(e.value, e.compared, false, Option.empty, e.hidden)
    def compare: ElementInfo[K] =
      ElementInfo(e.value, true, e.selected, e.label, e.hidden)
    def show: ElementInfo[K] =
      ElementInfo(e.value, e.compared, e.selected, e.label, false)
    def hide: ElementInfo[K] =
      ElementInfo(e.value, e.compared, e.selected, e.label, true)

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

  def apply[K](value: K,
               compared: Boolean,
               selected: Boolean,
               label: Option[String],
               hidden: Boolean): ElemInfo[K] =
    ElemInfo(value, compared, selected, label, hidden)

  def newInfo[K](value: K): ElemInfo[K] =
    ElemInfo(value, false, false, Option.empty, false)
