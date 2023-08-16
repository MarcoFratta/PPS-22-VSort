package model.properties

import ai.dragonfly.math.stats.probability.distributions.{Gaussian, Uniform}
import model.component.Params.Max

import scala.compiletime.ops.int.Min

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

private class BasicGenerator[T: Generable](ff: Int => Double) extends Generator[T]:
  override def f(x: Int): Double = ff(x)

trait HasRange(a: Int, b: Int):
  def min: Int = math.min(a, b)

  def max: Int = math.max(a, b)

trait Multiplied[T: Generable](r:Double) extends Generator[T] with HasRange:
  abstract override def f(x: Int): Double = super.f(x) * r

  abstract override def max: Int = (super.max * r).ceil.intValue

  abstract override def min: Int = (super.min* r).floor.intValue

trait Shifted[T](min: Double, max: Double) extends Generator[T] with HasRange:
  abstract override def f(x: Int): Double =
    min + (((super.f(x) - super.min) * (max - min)) / (super.max - super.min))


class GaussianGen[T: Generable](mean: Double, std: Double) extends Generator[T]
  with HasRange(0, 1):
  private val d = Gaussian(mean, std * std)

  override def f(x: Int): Double = d.p(x)

class UniformGen[T: Generable](a:Int,b:Int) extends BasicGenerator[T](x =>
  (math.min(a,b) + x) % math.max(a,b))
  with HasRange(math.min(a,b), math.max(a,b))