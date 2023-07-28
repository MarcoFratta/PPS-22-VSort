package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import model.seqProperties.Distributions.*
import model.seqProperties.Generable
import model.seqProperties.Modifier.countDuplicates


class DistributionsTest extends AnyFlatSpec with Matchers:

  given Generable[Int] = x => x.toInt

  "A uniform distribution with 0% of duplicates" should "have 0 duplicates" in {
    val g = UniformDistribution(1,100,0)
    val seq = g.generateAll(1 to 20)
    seq.size shouldBe 20
    countDuplicates(seq.values) shouldBe 0
  }

  "A uniform distribution with 100% of duplicates" should "have 20 duplicates" in {
    val g = UniformDistribution(1, 100, 100)
    val seq = g.generateAll(1 to 20)
    seq.size shouldBe 20
    countDuplicates(seq.values) shouldBe 20
  }

  "A uniform distribution with 50% of duplicates" should "have 10 duplicates" in {
    val g = UniformDistribution(1, 100, 50)
    val seq = g.generateAll(1 to 20)
    (50.doubleValue / 100) shouldBe 0.5
    countDuplicates(seq.values) shouldBe 10
  }
