package model.SeqProperties

import ai.dragonfly.math.stats.probability.distributions.{Gaussian, Uniform}

trait Generable[A]:
  def convert(x: Double): A

object Generable:
  def convert[T: Generable](x: Double): T =
    summon[Generable[T]].convert(x)

trait Generator[T: Generable]:

  def f(x: Int): Double

  def generateAll(range: Range): Map[Int, T] =
    Map.from(Seq.iterate(range.start, range.size)(x => x + range.step).map(x => (x, generate(x))))

  def generate(x: Int): T = convert(f(x))

  private def convert(a: Double): T = Generable.convert(a)

private case class BasicGenerator[T: Generable](ff: Int => Double) extends Generator[T]:
  override def f(x: Int): Double = ff(x)

trait HasRange(a: Int, b: Int):
  def min: Int = math.min(a, b)

  def max: Int = math.max(a, b)

trait ToPercentage[T: Generable] extends Generator[T]:
  abstract override def f(x: Int): Double = super.f(x) * 100

trait Shifted[T](min: Double, max: Double) extends Generator[T] with HasRange:
  abstract override def f(x: Int): Double =
    min + (((super.f(x) - super.min) * (max - min)) / (super.max - super.min))


class GaussianGen[T: Generable](mean: Double, std: Double) extends Generator[T]
  with HasRange(0, 1):
  private val d = Gaussian(mean, std * std)

  override def f(x: Int): Double = d.p(x)

class UniformGen[T: Generable](from: Int, to: Int) extends BasicGenerator[T](x => (x + from) % to)
  with HasRange(from, to)


object PercentageGen:

  def apply[T: Generable](generator: Generator[T]): Generator[T] with ToPercentage[T] =
    WithPercentage(generator.f)

  private class WithPercentage[T: Generable](f: Int => Double) extends BasicGenerator(f) with ToPercentage[T]





