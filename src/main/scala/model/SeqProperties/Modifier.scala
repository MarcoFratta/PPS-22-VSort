package model.SeqProperties

import ai.dragonfly.math.stats.probability.distributions.*

import scala.annotation.{tailrec, targetName}
import scala.util.Random

object Modifier:

  import scala.util.Random.*

  def countDuplicates[T](s: Iterable[T]): Int = s.size match
    case 0 => 0
    case 1 => 1
    case _ => s.count(x => isDuplicated(s, x))

  def isDuplicated[T](l: Iterable[T], x: T): Boolean =
    l.count(_ == x) >= 2

  private def getRandomValue[T](s: Iterable[T]): Option[T] =
    if s.isEmpty then Option.empty else Option(s.toList(Random.nextInt(s.size)))

  private def getRandomIndex[T](s: Seq[T]): Option[Int] =
    if s.isEmpty then Option.empty else Option(Random.nextInt(s.length))

  trait Duplicated[T: Generable](@targetName("percentage") % : Double) extends Generator[T]:
    if % > 1 || % < 0 then
      throw new IllegalArgumentException("percentage must be between 0 and 1")


    def duplicated(l: Map[Int, T]): Map[Int, T] =
      val n = (l.size * %).floor.toInt
      if l.size > 1 then duplicate(l)(if n > 1 then n else 0) else l

    override def generateAll(range: Range): Map[Int, T] =
      duplicated(super.generateAll(range))

    @tailrec
    private def duplicate(m: Map[Int, T])(n: Int): Map[Int, T] = countDuplicates(m.values) match
      case `n` => m
      case d if d > n => duplicate(removeDuplicate(m))(n)
      case d if n - d == 1 => duplicate(duplicatedIndex(m).fold(m)(x =>
        m.updated(nearestNonDuplicated(m, x).fold(x)(k => k), m(x))))(n)
      case _ => duplicate(nonDuplicatedIndex(m).fold(m)(x =>
        m.updated(x, m(nearestX(m, x)))))(n)

    private def nearestNonDuplicated(m: Map[Int, T], x: Int): Option[Int] =
      nearest(m, x)(x => !isDuplicated(m.values, m(x)))

    private def nonDuplicatedIndex(m: Map[Int, T]): Option[Int] =
      getRandomValue(m.filter((x, y) => super.generate(x) == y && !isDuplicated(m.values, y)).keys)

    private def duplicatedIndex(m: Map[Int, T]): Option[Int] =
      getRandomValue(m.filter((x, y) => isDuplicated(m.values, y)).keys)

    private def nearest(m: Map[Int, T], x: Int)(f: Int => Boolean): Option[Int] =
      val keys = m.keys.toList.ordered
      val kx = keys.indexOf(x)
      keys.slice(0, kx).findLast(f).fold(keys.slice(kx, keys.size).find(f))(x => Some(x))

    private def nearestX(m: Map[Int, T], x: Int): Int =
      val keys = m.keys.toList.ordered
      val f = nearest(m, x)
      (if keys.indexOf(x) >= keys.size / 2 then f(_ <= keys.size / 2) else f(_ >= keys.size / 2)).get

    private def removeDuplicate(m: Map[Int, T]): Map[Int, T] =
      val s = m.filter((x, y) => y != super.generate(x) && !isDuplicated(m.values, super.generate(x)))
      getRandomValue(s.keys).fold(m)(x => m.updated(x, super.generate(x)))


  extension[T] (s: Seq[T])
    def ordered(using o: Ordering[T]): Seq[T] = s.sorted



object Setters:
  extension (seq: Seq[Double])

    def shift(min: Int, max: Int): Seq[Double] =
      normalize(seq.min, seq.max, min, max)

    def roundToNDecimal(n: Int): Seq[Double] =
      seq.map(x => roundDouble(x, n))
    private def roundDouble(v: Double, n: Int): Double =
      BigDecimal(v).setScale(n, BigDecimal.RoundingMode.HALF_UP).toDouble

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
    val d = Gaussian(mean, std*std)
    generate(d.p(_))

  def exponentialDistribution(rate: Double, max: Double): Seq[Double] =
    generate(x => math.log(1 - Seq(x.toDouble).normalize(0, max,0, 1).head) / (-rate))

  def uniformDistribution(min: Int, max: Int): Seq[Double] =
    val d = Uniform(min, max)
    generate(_ => d.random())

private def generate[T](f: Int => T): Seq[T] =
  LazyList.iterate(0)(_ + 1).map(x => f(x))