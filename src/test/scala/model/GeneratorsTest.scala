package model

import model.SeqProperties.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GeneratorsTest extends AnyFlatSpec with Matchers:

  given Generable[Int] = x => x.toInt

  "a gaussian generator" should "exists" in {
    val g = GaussianGen(0, 1)
  }

  "a gaussian generator" should "generate a value between 0 and 1" in {
    val g = GaussianGen(4, 15)
    val y = g.generate(5)
    y should be >= 0
    y should be <= 1
  }

  "a gaussian generator" should "generate a double value" in {
    given Generable[Double] = x => x

    val g = GaussianGen[Double](4, 15)
    val y = g.generate(0)
    y shouldBe a[Double]
  }

  "a gaussian generator" should "generate a string value" in {
    given Generable[String] = x => x.toString

    val g = GaussianGen(4, 15)
    val y = g.generate(0)
    y shouldBe a[String]
  }

  "a gaussian generator" should "generate multiple values between 0 and 1" in {
    given Generable[Double] = x => x

    val g = GaussianGen[Double](4, 15)
    val y = g.generateAll(5 to 700)
    y.foreach(println(_))
    y.forall((x, y) => y >= 0 && y <= 100) shouldBe true
  }


  "a uniform generator" should "exists" in {
    val g = UniformGen(0, 100)
  }

  "a uniform generator in [5,50]" should "generate a value between 5 and 50" in {
    val g = UniformGen(5, 50)
    val y = g.generate(0)
    y should be >= 5
    y should be <= 50
  }

  "a uniform generator" should "generate a double value" in {
    given Generable[Double] = x => x

    val g = UniformGen(4, 15)
    val y = g.generate(0)
    y shouldBe a[Double]
  }

  "a uniform generator" should "generate a string value" in {
    given Generable[String] = x => x.toString

    val g = UniformGen(4, 15)
    val y = g.generate(0)
    y shouldBe a[String]
  }

  "a uniform generator [0,50]" should "generate multiple values between 0 and 50" in {
    given Generable[Double] = x => x.toInt

    val g = UniformGen(0, 50)
    val y = g.generateAll(0 to 100000 by 100)
    y.foreach(println(_))
    y.forall((x, y) => y >= 0 && y <= 50) shouldBe true
  }




