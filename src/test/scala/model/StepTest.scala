package model

import model.api.Step
import model.api.Step.*
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StepTest extends AnyFlatSpec with Matchers {

    "A swap step" should "exists" in {
        val step = Step.Swap(0,1)
    }

    "A comparison step" should "exists" in {
        val step = Step.Comparison(0, 1)
    }

    "A selection step" should "exists" in {
        val step = Step.Selection("test", 0)
    }

    "A deselection step" should "exists" in {
        var step = Step.Deselection("test")
    }

    "A divide step" should "exists" in {
        val step = Step.Divide(0, 1)
    }

}
