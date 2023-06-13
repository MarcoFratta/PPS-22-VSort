package SetProperties

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class testSeqProperties extends AnyFlatSpec with Matchers:
  behavior of "Set properties of starter values"
  
  it should "exists" in {
    val starterSeq = SetBuilder().build()
  }
  "When created without parameters, it" should "have default values" in {
    val starterSeq1 = SetBuilder().build()
    val defaultMinValue = 0
    val defaultMaxValue = 100
    val defaultSizeValue = 20
    starterSeq1.max should be <= defaultMaxValue
    starterSeq1.min should be >= defaultMinValue
    starterSeq1 should have size defaultSizeValue
  }
  
