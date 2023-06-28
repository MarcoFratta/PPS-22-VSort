package model

import model.SeqProperties.Modifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DistributionBehaviourTest extends AnyFlatSpec with Matchers:

  import Modifier.*
  import model.SeqProperties.Generators.*
  import model.SeqProperties.Setters.*

  "A sequence (7,6,4) with ascending ordering distribution" should "be (4,6,7)" in {
    val seq = Seq(7,6,4).ordered(using scala.Ordering.Int)
    seq shouldEqual Seq(4,6,7)
  }

  "A sequence (7,4,6) with descending order distribution" should "be (7,6,4)" in {
    val seq = Seq(7, 4, 6).ordered(using scala.Ordering.Int.reverse)
    seq shouldEqual Seq(7, 6, 4)
  }

  "A sequence (0.3, 0.5, 1.0) with values between 0 and 5" should "be (0.0, 1.4, 5.0)" in {
    val seq = Seq(0.3, 0.5, 1.0).shift(0,5)
    seq.roundToNDecimal(1) shouldEqual Seq(0.0, 1.4, 5.0)
  }

  "A sequence (0.3, -0.8, 0.5, -0.3, 1.0) with values between 10 and 20" should "be (16.1, 10.0, 17.2, 12.8, 20.0)" in {
    val seq = Seq(0.3, -0.8, 0.5, -0.3, 1.0).shift(10, 20)
    seq.roundToNDecimal(1) shouldEqual Seq(16.1, 10.0, 17.2, 12.8, 20.0)
  }
  "A sequence (11.0, 25.0, 15.0, 30.0) with min value = 20" should "be (20.0, 27.5, 22.5, 30.0)" in {
    val seq = Seq(10.0, 25.0, 15.0, 30.0).shift(20, 30)
    seq.roundToNDecimal(1) shouldEqual Seq(20.0, 27.5, 22.5, 30.0)
  }

  "A sequence (11.0, 25.7, 15.2, 31.6)" should "be (11, 26, 15, 32) after conversion in integer" in {
    val seq = Seq(11.0, 25.1, 15.2, 31.6).doubleToInt
    seq shouldEqual Seq(11, 25, 15, 31)
  }

  "A Gaussian distributed seq with mean 4 and std 15" should "be equal to seq2" in{
    val seq = normalDistribution(4,15).take(10)
    val seq2 = Seq(0.0257, 0.0261, 0.0264, 0.0265, 0.0266, 0.0265, 0.0264, 0.0261, 0.0257, 0.0252)
    seq.roundToNDecimal(4) shouldEqual seq2
  }

  "A Gaussian distributed seq with mean 4 and std 15 and values between 0 and 100" should "have values between 0 and 100" in {
    val seq = normalDistribution(4, 15).take(10).shift(0, 100)
    seq.max should be <= 100.1
    seq.min should be >= 0.0
  }

  "A uniform distributed seq " should "have values between 0 and 1" in {
    val seq = uniformDistribution(0, 1).take(1000)
    all(seq) should (be >= 0.0 and be <= 1.0)
  }
  "A uniform distributed seq with max 500 " should "have values between 0 and 500" in {
    val seq = uniformDistribution(0, 500).take(10)
    all(seq) should (be >= 0.0 and be <= 500.0)
  }
  "A uniform distributed seq with max 500 and min 100 " should "have values between 100 and 500" in {
    val seq = uniformDistribution(100, 500).take(10)
    all(seq) should (be >= 100.0 and be <= 500.0)
  }
/*
  "A exponential distributed seq with rate 0.8 " should "have a rate >= 0.7 and <= 0.9" in {
    val rateFromula: Seq[Double] => Double = s => 1 / (s.sum / s.size)
    val n = 100
    var sum = 0.0
    for _ <- 0 to n do
      val seq = exponentialDistribution(0.8, 5).take(100).sorted
      sum = sum + rateFromula(seq)

    sum = sum / n
    println(f"sum -> $sum")
    sum should be >= 0.7
    sum should be <= 0.9
  }
*/