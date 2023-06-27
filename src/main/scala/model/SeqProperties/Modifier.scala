package model.SeqProperties

import ai.dragonfly.math.stats.probability.distributions.*

import scala.util.Random

object Modifier:
  extension[T] (s: Seq[T])
    def ordered(using o: Ordering[T]): Seq[T] = s.sorted
    def duplicated(rate: Double): Seq[T] = ???

object Setters:
  extension (seq: Seq[Double])

    def shift(min: Int, max: Int): Seq[Double] =
      normalize(seq.min, seq.max, min, max)

    def roundToFirstDecimal(): Seq[Double] =
      seq.map(x => roundDouble(x))
    private def roundDouble(v: Double): Double =
      BigDecimal(v).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble

    def normalize(oldMin: Double, oldMax: Double, newMin: Double, newMax: Double): Seq[Double] =
      val oldStep = oldMax - oldMin
      val newStep = newMax - newMin
      seq.map(a => (((a - oldMin) * newStep) / oldStep) + newMin)

    def doubleToInt: Seq[Int] =
      seq.map(a => a.toInt)
      
object Generators:

  import Setters.*


  def betaDistribution(alfa: Double, beta: Double, min: Double = 0.0, max: Double = 1.0): Seq[Double] =
    val d = Beta(alfa, beta, min, max)
    generate(d.p(_))

  def normalDistribution(mean: Double, std: Double): Seq[Double] =
    val d = Gaussian(mean, std)
    generate(d.p(_))

  def exponentialDistribution(rate: Double, max: Double): Seq[Double] =
    generate(x => math.log(1 - Seq(x.toDouble).normalize(0, max,0, 1).head) / (-rate))

  def uniformDistribution(min: Int, max: Int): Seq[Double] =
    val d = Uniform(min, max)
    generate(d.p(_))

private def generate[T](f: Int => T): Seq[T] =
  LazyList.iterate(0)(_ + 1).map(x => f(x))