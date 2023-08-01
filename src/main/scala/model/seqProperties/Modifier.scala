package model.seqProperties

import ai.dragonfly.math.stats.probability.distributions.*

import scala.annotation.{tailrec, targetName}
import scala.util.Random

object Modifier:

  import scala.util.Random.*

  def countDuplicates[T](s: Iterable[T]): Int = s.size match
    case n if n <= 1 => n
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
      println(f"Duplicating")
      val n = (l.size * %).floor.toInt
      println(f"Duplicated number $n")
      if l.size > 1 then duplicate(l)(if n > 1 then n else 0) else l

    override def generateAll(range: Range): Map[Int, T] =
      println(f"Generating with duplicates ${this.%}")
      duplicated(super.generateAll(range))

    @tailrec
    private def duplicate(m: Map[Int, T])(n: Int): Map[Int, T] = countDuplicates(m.values) match
      case `n` => m
      case d if d > n =>
        println(f"removing duplicated $d")
        duplicate(removeDuplicate(m))(n)
      case d if n - d == 1 =>
        println(f"adding last duplicated $d")
        duplicate(duplicatedIndex(m).fold(m)(x =>
        m.updated(nearestNonDuplicated(m, x).fold(x)(k => k), m(x))))(n)
      case x =>
        println(f"adding duplicated $x")
        duplicate(nonDuplicatedIndex(m).fold(m)(x =>
        m.updated(x, m(nearestX(m, x)))))(n)

    private def nearestNonDuplicated(m: Map[Int, T], x: Int): Option[Int] =
      nearest(m, x)(x => !isDuplicated(m.values, m(x)))

    private def nonDuplicatedIndex(m: Map[Int, T]): Option[Int] =
      getRandomValue(m.filter((x, y) => super.generate(x) == y && !isDuplicated(m.values, y)).keys)

    private def duplicatedIndex(m: Map[Int, T]): Option[Int] =
      getRandomValue(m.filter((x, y) => isDuplicated(m.values, y)).keys)

    private def nearest(m: Map[Int, T], x: Int)(f: Int => Boolean): Option[Int] =
      val keys = m.keys.toVector.sorted
      val kx = keys.indexOf(x)
      keys.slice(0, kx).findLast(f).fold(keys.slice(kx, keys.size).find(f))(x => Some(x))

    private def nearestX(m: Map[Int, T], x: Int): Int =
      val keys = m.keys.toVector.sorted
      val f = nearest(m, x)
      (if keys.indexOf(x) >= keys.size / 2 then f(_ <= keys.size / 2) else f(_ >= keys.size / 2)).get

    private def removeDuplicate(m: Map[Int, T]): Map[Int, T] =
      val s = m.filter((x, y) => y != super.generate(x) && !isDuplicated(m.values, super.generate(x)))
      val removable = if s.isEmpty then m.filter((x, y) => isDuplicated(m.values, super.generate(x))) else s
      println(f"removable $removable")
      val res = getRandomValue(removable.keys).fold(m)(x => m.updated(x, super.generate(x)))
      val oldDuplicated = countDuplicates(m.values)
      if countDuplicates(res.values) < oldDuplicated then res
      else throw new IllegalArgumentException(f"The distribution cannot have less then $oldDuplicated duplicates")