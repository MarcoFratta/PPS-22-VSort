package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import model.SeqProperties.Distribution

class DistributionBehaviourTest extends AnyFlatSpec with Matchers:

  import Distribution.*

  "A sequence (7,6,4) with ascending ordering distribution" should "be (4,6,7)" in {
    val seq = Seq(7,6,4).ordered(using scala.Ordering.Int)
    assert(seq == Seq(4,6,7))
  }

  "A sequence (7,4,6) with descending order distribution" should "be (7,6,4)" in {
    val seq = Seq(7, 4, 6).ordered(using scala.Ordering.Int.reverse)
    assert(seq == Seq(7, 6, 4))
  }

