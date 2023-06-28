package model

import model.SeqProperties.*
import model.SeqProperties.Generators.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GeneratorsTest extends AnyFlatSpec with Matchers:


  given Generable[Int] = x => (x * 100).toInt

  "a gaussian generator" should "exists" in {
    val g = GaussianGen(0, 1)
  }

  "a gaussian generator" should "generate a value" in {
    val g = GaussianGen(4, 15)
    val y = g.generate(5)
    y should be >= 0
    y should be <= 100
  }

  "a gaussian generator" should "generate a double value" in {
    given Generable[Double] = x => x

    val g = GaussianGen[Double](4, 15)
    val y = g.generate(0)
    y shouldBe a[Double]
  }

  "a gaussian generator" should "generate values between 0 and 1" in {
    given Generable[Double] = x => x

    val g = GaussianGen[Double](4, 15)
    val y = g.generateAll(300)
    y.forall(x => x >= 0 && x <= 100) shouldBe true
  }



