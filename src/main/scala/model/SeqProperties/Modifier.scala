package model.SeqProperties
import scala.util.Random
object Modifier:
  extension [T](s:scala.Seq[T])
    def ordered(using o:Ordering[T]):scala.Seq[T] = s.sorted
object Setters:
  extension (seq:Seq[Double])

    def shift(min:Int, max: Int): Seq[Double] =
     normalize(seq.min, seq.max,min, max)

    private def roundDouble(v: Double): Double =
      BigDecimal(v).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble

    def normalize(oldMin: Double, oldMax: Double, newMin: Double, newMax: Double): Seq[Double] =
      val oldStep = oldMax - oldMin
      val newStep = newMax - newMin
      seq.map(a => roundDouble(((a - oldMin) * newStep) / oldStep) + newMin)

      
object Generators:
  import Setters.*
  def normalDistribution(mean:Double, std:Double):scala.Seq[Double] =
    generate(Random.nextGaussian())(x => x * std + mean)

  def uniformDistribution(min:Int, max:Int): scala.Seq[Double] =
    generate(Random.nextDouble())(x => Seq(x).normalize(0,1,min,max).head)

private def generate[T](g: => T)(f: T => T): scala.Seq[T] =
  LazyList.iterate(f(g))(x => f(g))