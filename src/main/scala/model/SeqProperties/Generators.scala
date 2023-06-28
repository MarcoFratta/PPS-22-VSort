package model.SeqProperties

import ai.dragonfly.math.stats.probability.distributions.Gaussian

trait Generable[A]:
  def convert(x: Double): A

object Generable:
  def convert[T: Generable](x: Double): T =
    summon[Generable[T]].convert(x)

trait Generator[T: Generable]:

  def f(x: Int): Double

  def generateAll(n: Int): Seq[T] =
    Seq.iterate(0, n)(_ + 1).map(generate)

  def generate(x: Int): T = convert(f(x))

  private def convert(a: Double): T = Generable.convert(a)

private case class BasicGenerator[T: Generable](ff: Int => Double) extends Generator[T]:
  override def f(x: Int): Double = ff(x)

trait HasRange:
  def range: Range

trait ToPercentage[T: Generable] extends Generator[T]:
  abstract override def f(x: Int): Double = super.f(x) * 100

trait Shifted[T](min: Double, max: Double) extends Generator[T] with HasRange:
  abstract override def f(x: Int): Double =
    min + (((super.f(x) - range.start) * (max - min)) / range.size)

object GaussianGen:
  def apply[T: Generable](mean: Double, std: Double): Generator[T] = new Generator[T]:
    private val d = Gaussian(mean, std * std)

    override def f(x: Int): Double = d.p(x)


object PercentageGen:

  def apply[T: Generable](generator: Generator[T]): Generator[T] with ToPercentage[T] =
    WithPercentage(generator.f)

  private class WithPercentage[T: Generable](f: Int => Double) extends BasicGenerator(f) with ToPercentage[T]





