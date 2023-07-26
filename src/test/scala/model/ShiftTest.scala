package model

import model.seqProperties.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShiftTest extends AnyFlatSpec with Matchers:

  "a gaussian generator shifted to 50 - 120" should "generate a values between 50 and 120" in {
    given Generable[Int] = x => x.toInt

    case class ShiftedGaussian[T: Generable]()
      extends GaussianGen[T](4, 15)
        with Shifted[T](50, 120)

    val g = ShiftedGaussian[Int]()
    val y = g.generateAll(0 to 1000 by 5)
    y.forall((_, y) => y >= 50) shouldBe true
    y.forall((_, y) => y <= 120) shouldBe true
  }

  "a gaussian generator shifted to (-1) - (-50)" should "generate a values between -1 and -50" in {
    given Generable[Int] = x => x.toInt

    case class ShiftedGaussian[T: Generable]()
      extends GaussianGen[T](4, 15)
        with Shifted[T](-1, -50)

    val g = ShiftedGaussian[Int]()
    val y = g.generateAll(0 to 1000 by 5)
    y.forall((_, y) => y >= -50) shouldBe true
    y.forall((_, y) => y <= -1) shouldBe true
  }