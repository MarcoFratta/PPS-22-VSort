package model

import model.seqProperties.*
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.targetName

class ModifierTest extends AnyFlatSpec with Matchers with PrivateMethodTester:

  import Modifier.*

  given Generable[Int] = x => x.toInt


  "the value 5" should "be duplicated in seq(1,2,3,4,5,3,6,5)" in {
    isDuplicated(Seq(1, 2, 3, 4, 5, 3, 6, 5), 5) shouldBe true
  }

  "the value 0" should "not be duplicated in seq(1,2,3,4,5,3,6,5)" in {
    isDuplicated(Seq(1, 2, 3, 4, 5, 3, 6, 5), 0) shouldBe false
  }

  "count duplicated on an empty seq " should "give 0" in {
    countDuplicates(Seq()) shouldBe 0
  }

  "count duplicated on seq(1, 1, 2, 3, 4, 4, 5, 1)" should "give 5" in {
    countDuplicates(Seq(1, 1, 2, 3, 4, 4, 5, 1)) shouldBe 5
  }

  class UniformWithDuplicates[T: Generable](min: Int, max: Int, @targetName("Rate") % : Double) extends
    UniformGen[T](min, max) with Duplicated[T](%)

  "A sequence with 50% of duplicates" should
    "have 5 duplicated values" in {

    val g = UniformWithDuplicates(10, 100, 0.5)
    val seq = g.generateAll(1 to 10)

    countDuplicates(seq.values) shouldBe 5
  }

  "A sequence with 100 elements with 50% of duplicates" should
    "have 50 duplicated values" in {
    val g = UniformWithDuplicates(10, 100, 0.5)
    val seq = g.generateAll(1 to 100)

    countDuplicates(seq.values) shouldBe 50
  }

  "A sequence with 10 elements with 100% of duplicates" should
    "have 10 duplicated values" in {
    val g = UniformWithDuplicates(10, 100, 1)
    val seq = g.generateAll(1 to 10)
    countDuplicates(seq.values) shouldBe 10
  }

  "A sequence with 100 elements with 0% of duplicates" should
    "have 0 duplicated values" in {
    val g = UniformWithDuplicates(10, 100, 0.0)
    val seq = g.generateAll(1 to 100)
    countDuplicates(seq.values) shouldBe 0
  }

  "A sequence with 100 elements with 1% of duplicates" should
    "have 0 duplicated values" in {
    val g = UniformWithDuplicates(10, 100, 0.01)
    val seq = g.generateAll(1 to 100)
    countDuplicates(seq.values) shouldBe 0
  }

  "A sequence with 100 elements with 5% of duplicates" should
    "have 0 duplicated values after calling 0% duplicates" in {
    val g = UniformWithDuplicates(0, 1000, 0.05)
    val seq = g.generateAll(1 to 1000)
    val seq2 = UniformWithDuplicates(0, 1000, 0).duplicated(seq)
    countDuplicates(seq.values) shouldBe 50
    countDuplicates(seq2.values) shouldBe 0
  }

  "A sequence with 100 elements with 25% of duplicates" should
    "have 15 duplicated values after calling 15% duplicates" in {
    val g = UniformWithDuplicates(0, 100, 0.25)
    val seq = g.generateAll(1 to 100)
    val seq2 = UniformWithDuplicates(0, 100, 0.15).duplicated(seq)
    countDuplicates(seq.values) shouldBe 25
    countDuplicates(seq2.values) shouldBe 15
  }

  "A sequence with 500% of duplicates" should "fail" in {
    the[IllegalArgumentException] thrownBy {
      val g = UniformWithDuplicates(0, 1000, 5)
      val seq = g.generateAll(1 to 1000)
      fail()
    } should have message "percentage must be between 0 and 1"
  }

  "A sequence with negative % of duplicates" should "fail" in {
    the[IllegalArgumentException] thrownBy {
      val g = UniformWithDuplicates(0, 1000, -0.6)
      val seq = g.generateAll(1 to 1000)
      fail()
    } should have message "percentage must be between 0 and 1"
  }

  "A gaussian distribution with 100 values and 100 % of duplicates" should "have 100 duplicates"in {
    case class DistributionTest[T: Generable](mean: Int, std: Int, mi: Int, ma: Int, percentage: Int)
      extends GaussianGen[T](mean, std)
        with Shifted[T](mi, ma)
       with Duplicated((percentage / 100).toDouble.floor)


    val g = DistributionTest(0, 1, 1, 10000, 100)
    val seq = g.generateAll(1 to 100)
    countDuplicates(seq.values) shouldBe 100
  }

  "A gaussian distribution with 100 values and 50 % of duplicates" should "fail" in {
    case class DistributionTest[T: Generable](mean: Int, std: Int, mi: Int, ma: Int, percentage: Int)
      extends GaussianGen[T](mean, std)
        with Shifted[T](mi, ma)
        with Duplicated((percentage / 100).toDouble.floor)


    the[IllegalArgumentException] thrownBy {
      val g = DistributionTest(0, 1, 1, 10000, 50)
      val seq = g.generateAll(1 to 100)
      fail()
    } should have message "The distribution cannot have less then 96 duplicates"
  }


