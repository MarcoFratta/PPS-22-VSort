package model

import scala.util.{Failure, Success, Try}




case class MemoryList[T](data: Seq[T], steps: Seq[Step]):

  def swap(a: Int, b: Int): Try[MemoryList[T]] =
    Try(MemoryList(swapElements(a, b), addStep(Step.Swap(a, b))))


  def select(a: Int): Try[MemoryList[T]]=
    Try(MemoryList(data, addStep(Step.Selection(a))))

  def deselect(a: Int): Try[MemoryList[T]] =
    Try(MemoryList(data, addStep(Step.Deselection(a))))

  def compare(a: Int, b: Int)(ifTrue: MemoryList[T] => MemoryList[T])
             (ifFalse: MemoryList[T] => MemoryList[T])(using f: (T,T) => Int): MemoryList[T] =
    val memoryList = MemoryList(data, steps :+ Step.Comparison(a,b))
    if f(data(a), data(b)) > 0 then ifTrue(memoryList) else ifFalse(memoryList)


  def length():Int = data.size
  private def swapElements(a: Int, b: Int) : Seq[T] =
    data updated (a, data.toList(b)) updated (b, data.toList(a))
