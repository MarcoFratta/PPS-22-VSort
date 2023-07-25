package model

import model.seqProperties.{GaussianGen, Generable, Multiplied, Shifted}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MultiplierTest extends AnyFlatSpec with Matchers:

  case class PercentageGaussian[T: Generable]()
    extends GaussianGen[T](4, 15)
      with Multiplied[T](100)

  case class RangeGaussian[T: Generable]()
    extends GaussianGen[T](100, 20)
      with Multiplied[T](10000)
      with Shifted[T](1, 100)



  "a gaussian generator(4,15) with percentage" should "generate 2 with x = 0" in {
    given Generable[Int] = x => x.toInt
    val g = PercentageGaussian()
    val y = g.generate(0)
    y shouldBe 2
  }

  "a gaussian generator with percentage" should "generate values between 0 and 500" in {
    given Generable[Int] = x => x.toInt

    val g = RangeGaussian()
    val y = g.generateAll(50 to 150).toList.sortWith((a,b) => a._1 <= b._1)
    y.foreach(a => println(a))
    y.forall((x, y) => y >= 0 && y <= 100) shouldBe true
  }