package model

import model.SeqProperties.{GaussianGen, Generable, PercentageGen}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PercentageGeneratorsTest extends AnyFlatSpec with Matchers:

  "a gaussian generator(4,15) with percentage" should "generate 2 with x = 0" in {
    given Generable[Int] = x => x.toInt

    val g = PercentageGen(GaussianGen(4, 15))
    val y = g.generate(0)
    y shouldBe 2
  }

  "a gaussian generator with percentage" should "generate values between 0 and 100" in {
    given Generable[Int] = x => x.toInt

    val g = GaussianGen(4, 15)
    val y = g.generateAll(300)
    y.forall(x => x >= 0 && x <= 100) shouldBe true
  }