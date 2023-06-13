package model.SeqProperties

import scala.util.Random
object Modifier:
  extension [T](s:scala.Seq[T])
    def ordered(using o:Ordering[T]):scala.Seq[T] = s.sorted



object Generators:
  def normalDistribution(mean:Double, std:Double):scala.Seq[Double] =
    generate(Random.nextGaussian())(x => x * std + mean)

  def uniformDistribution(): scala.Seq[Double] =
    generate(Random.nextDouble())(x => x)

private def generate[T](g: => T)(f: T => T): scala.Seq[T] =
  LazyList.iterate(f(g))(x => f(g))