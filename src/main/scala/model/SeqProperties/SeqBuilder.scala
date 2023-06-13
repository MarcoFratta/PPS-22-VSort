// create enum/ class for type of distriubution
package model.SeqProperties

abstract class Seq[T]:
  def max: Int
  def min: Int
  def size: Int
  def build()=
    Array.fill(size)(new scala.util.Random().between(min, max)).toSeq
  
case class BasicSeq() extends Seq[Int]:
  override def max: Int = 100

  override def min: Int = 0

  override def size: Int = 20
trait Max[T](val maxVal:Int) extends Seq[T]:
  override def max: Int = maxVal

trait Min[T](val minVal: Int) extends Seq[T]:
  override def min: Int = minVal
  
trait Size[T](val length: Int) extends Seq[T]:
  override def size: Int = length

