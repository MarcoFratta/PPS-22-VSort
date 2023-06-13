package SeqProperties

import model.SeqProperties.SeqBuilder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class testSeqProperties extends AnyFlatSpec with Matchers:
  behavior of "Set properties of starter values"
  
  it should "exists" in {
    val starterSeq = SeqBuilder().build()
  }
  "When created without parameters, it" should "have default values" in {
    val starterSeq1 = SeqBuilder().build()
    val defaultMinValue = 0
    val defaultMaxValue = 100
    val defaultSizeValue = 20
    starterSeq1.max should be <= defaultMaxValue
    starterSeq1.min should be >= defaultMinValue
    starterSeq1 should have size defaultSizeValue
  }

  "When created with a max value, it" should "have max value" in {
    val maxValue = 30
    val starterSeq2 = SeqBuilder(max = maxValue).build()
    val defaultMinValue = 0
    val defaultSizeValue = 20
    starterSeq2.max should be <= maxValue
    starterSeq2.min should be >= defaultMinValue
    starterSeq2 should have size defaultSizeValue
  }

  "When created with a max, min value and a size, it" should "have these properties" in {
    val maxValue = 30
    val minValue = 10
    val size = 5
    val starterSeq3 = SeqBuilder(minValue, maxValue, size ).build()
    starterSeq3.max should be <= maxValue
    starterSeq3.min should be >= minValue
    starterSeq3 should have size size
  }
  
  "When created with a max value < min value, it" should "throw an exception" in {
    val maxValue = 30
    val minValue = 10
    val size = 5
    assertThrows[IllegalArgumentException]{
      val starterSeq4 = SeqBuilder(maxValue, minValue, size).build()
    }
  }
  "When created with a not valid size, it" should "throw an exception" in {
    val maxValue = 30
    val minValue = 10
    val size = -3
    assertThrows[IllegalArgumentException] {
      val starterSeq5 = SeqBuilder(maxValue, minValue, size).build()
    }
  }