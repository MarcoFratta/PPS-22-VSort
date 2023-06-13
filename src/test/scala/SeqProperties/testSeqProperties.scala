package SeqProperties

import model.SeqProperties.{BasicSeq, Max, Min, Size}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class testSeqProperties extends AnyFlatSpec with Matchers:
  behavior of "Set properties of starter values"
  
  it should "exists" in {
    BasicSeq().build()
  }
  "When created without parameters, it" should "have default values" in {
    val starterSeq1 = BasicSeq().build()
    val defaultMinValue = 0
    val defaultMaxValue = 100
    val defaultSizeValue = 20
    starterSeq1 match
      case Success(seq) =>
        seq.max should be <= defaultMaxValue
        seq.min should be >= defaultMinValue
        seq should have size defaultSizeValue
      case Failure(_) => fail()
  }

  "When created with a max value, it" should "have max value" in {
    val maxValue = 30
    class SeqWithProperties() extends BasicSeq() with Max[Int](maxValue)
    val starterSeq2 = new SeqWithProperties().build()
    val defaultMinValue = 0
    val defaultSizeValue = 20
    starterSeq2 match
      case Success(seq) =>
        seq.max should be <= maxValue
        seq.min should be >= defaultMinValue
        seq should have size defaultSizeValue
      case Failure(_) => fail()
  }

  "When created with a max, min value and a size, it" should "have these properties" in {
    val maxValue = 30
    val minValue = 10
    val size = 5
    class SeqWithProperties() extends BasicSeq() with Max[Int](maxValue) with Min[Int](minValue) with Size[Int](size)
    val starterSeq3 = new SeqWithProperties().build()
    starterSeq3 match
      case Success(seq) =>
        seq.max should be <= maxValue
        seq.min should be >= minValue
        seq should have size size
      case Failure(_) => fail()
  }
  
  "When created with a max value < min value, it" should "fail" in {
    val maxValue = 10
    val minValue = 30
    val size = 5
    class SeqWithProperties() extends BasicSeq() with Max[Int](maxValue) with Min[Int](minValue) with Size[Int](size)
    val starterSeq4 = new SeqWithProperties().build()
    starterSeq4 match
      case Failure(_) =>
      case Success(_) => fail()
  }
  "When created with a not valid size, it" should "fail" in {
    val maxValue = 30
    val minValue = 10
    val size = -3
    class SeqWithProperties() extends BasicSeq() with Max[Int](maxValue) with Min[Int](minValue) with Size[Int](size)
    val starterSeq4 = new SeqWithProperties().build()
    starterSeq4 match
      case Failure(_) =>
      case Success(_) => fail()
  }