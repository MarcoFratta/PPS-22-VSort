package SetProperties

import org.scalatest.funsuite.AnyFunSuite

class testSeqProperties extends AnyFunSuite:
  val defaultList = SetValues()

  test("A set without informations should have default values") {
    val actualList = SetBuilder().build()
    assertResult(defaultList)(actualList)
  }
