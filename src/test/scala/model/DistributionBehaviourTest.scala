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

  "A sequence (0.3, 0.5, 1.0) with values between 0 and 5" should "be (0.0, 1.4, 5.0)" in {
    val seq = Seq(0.3, 0.5, 1.0).shift(0,5)
    assert(seq == Seq(0.0, 1.4, 5.0))
  }

  "A sequence (0.3, -0.8, 0.5, -0.3, 1.0) with values between 10 and 20" should "be (16.1, 10.0, 17.2, 12.8, 20.0)" in {
    val seq = Seq(0.3, -0.8, 0.5, -0.3, 1.0).shift(10, 20)
    assert(seq == Seq(16.1, 10.0, 17.2, 12.8, 20.0))
  }
  "A sequence (11.0, 25.0, 15.0, 30.0) with min value = 20" should "be (20.0, 27.5, 22.5, 30.0)" in {
    val seq = Seq(10.0, 25.0, 15.0, 30.0).shift(20, 30)
    assert(seq == Seq(20.0, 27.5, 22.5, 30.0))
  }

  "A sequence (11.0, 25.7, 15.2, 31.6)" should "be (11, 26, 15, 32) after conversion in integer" in {
    val seq = Seq(11.0, 25.1, 15.2, 31.6).doubleToInt
    assert(seq == Seq(11, 25, 15, 31))
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

  "A Gaussian distributed seq with mean 30" should "have all positive values" in {


  }




  "A Gaussian distributed seq with mean 4 and std 15 and values between 0 and 100" should "have values between 0 and 100" in {
    val seq = normalDistribution(4, 15).take(10).shift(0,100)
    seq.foreach(x => print(x + " "))
    seq.max should be <= 100.0
    seq.min should be >= 0.0
  }

  "A uniform distributed seq " should "have values between 0 and 1" in {
    val seq = uniformDistribution(0,1).take(1000)
    assert(seq.forall(x => x >= 0 && x <= 1))
  }
  "A uniform distributed seq with max 500 " should "have values between 0 and 500" in {
    val seq = uniformDistribution(0,500).take(10)
    assert(seq.forall(x => x >= 0 && x <= 500))
  }
  "A uniform distributed seq with max 500 and min 100 " should "have values between 100 and 500" in {
    val seq = uniformDistribution(100,500).take(10)
    assert(seq.forall(x => x >= 100 && x <= 500))
  }