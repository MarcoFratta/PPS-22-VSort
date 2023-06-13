package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import model.SeqProperties.Modifier

class DistributionBehaviourTest extends AnyFlatSpec with Matchers:

  import Modifier.*
  import model.SeqProperties.Setters.*
  import model.SeqProperties.Generators.*

  "A sequence (7,6,4) with ascending ordering distribution" should "be (4,6,7)" in {
    val seq = Seq(7,6,4).ordered(using scala.Ordering.Int)
    assert(seq == Seq(4,6,7))
  }

  "A sequence (7,4,6) with descending order distribution" should "be (7,6,4)" in {
    val seq = Seq(7, 4, 6).ordered(using scala.Ordering.Int.reverse)
    assert(seq == Seq(7, 6, 4))
  }

  "A sequence (0.3, 0.5, 1.0) with max value = 5" should "be (1.5, 2.5, 5.0)" in {
    val seq = Seq(0.3, 0.5, 1.0).setMax(5)
    assert(seq == Seq(1.5, 2.5, 5.0))
  }

  "A sequence (0.3, -0.8, 0.5, -0.3, 1.0) with max value = 10" should "be ((6.5, 1.0, 7.5, 3.5, 10.0))" in {
    val seq = Seq(0.3, -0.8, 0.5, -0.3, 1.0).setMax(10)
    assert(seq == Seq(6.5, 1.0, 7.5, 3.5, 10.0))
  }
  "A sequence (11.0, 25.0, 15.0, 30.0) with min value = 20" should "be (20.0, 27.5, 22.5, 30.0)" in {
    val seq = Seq(10.0, 25.0, 15.0, 30.0).setMin(20)
    assert(seq == Seq(20.0, 27.5, 22.5, 30.0))
  }

  "A Gaussian distributed seq with mean 4 and std 15" should "have mean 4 and std 15" in{
    val seq = normalDistribution(4,15).take(1000)
    val avg = seq.sum / seq.size
    val std = scala.math.sqrt(seq.map(x => math.pow(x - avg, 2)).sum / seq.size)
    avg should be >= 3.0
    avg should be <= 5.0
    std should be >= 14.0
    std should be <= 16.0
  }
/*
  "A Gaussian distributed seq with mean 4 and std 15 and max 100" should "have values between 0 and 100" in {
    val seq = normalDistribution(4, 15).take(10).setMax(100)
    seq.foreach(x => print(x + " "))
    seq.max should be <= 100.0
    seq.min should be >= 0.0
  }
*/
  "A uniform distributed seq " should "have values between 0 and 1" in {
    val seq = uniformDistribution().take(1000)
    assert(seq.forall(x => x >= 0 && x <= 1))
  }
  "A uniform distributed seq with max 500 " should "have values between 0 and 500" in {
    val seq = uniformDistribution().take(10).setMax(500)
    assert(seq.forall(x => x >= 0 && x <= 500))
  }
  "A uniform distributed seq with max 500 and min 100 " should "have values between 100 and 500" in {
    val seq = uniformDistribution().take(10).setMax(500).setMin(100)
    assert(seq.forall(x => x >= 100 && x <= 500))
  }



