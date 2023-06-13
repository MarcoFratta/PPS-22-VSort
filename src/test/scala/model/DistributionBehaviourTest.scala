package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import model.SeqProperties.Modifier

class DistributionBehaviourTest extends AnyFlatSpec with Matchers:

  import Modifier.*
  import model.SeqProperties.Generators.*

  "A sequence (7,6,4) with ascending ordering distribution" should  "be (4,6,7)" in {
    val seq = Seq(7,6,4).ordered(using scala.Ordering.Int)
    assert(seq == Seq(4,6,7))
  }

  "A sequence (7,4,6) with descending order distribution" should "be (7,6,4)" in {
    val seq = Seq(7, 4, 6).ordered(using scala.Ordering.Int.reverse)
    assert(seq == Seq(7, 6, 4))
  }


  "A Gaussian distributed seq with mean 4 and std 15" should "have mean 2 and std 15" in{
    val seq = normalDistribution(4,15).take(500)
    val avg = seq.sum / seq.size
    val std = scala.math.sqrt(seq.map(x => math.pow(x - avg, 2)).sum / seq.size)
    avg should be >= 3.0
    avg should be <= 5.0
    std should be >= 14.0
    std should be <= 16.0
  }

  "A uniform distributed seq " should "have values between 0 and 1" in {
    val seq = uniformDistribution().take(150)
    assert(seq.forall(x => x >= 0 && x <= 1))
  }



