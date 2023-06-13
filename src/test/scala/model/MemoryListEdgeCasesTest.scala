package model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class MemoryListEdgeCasesTest extends AnyFlatSpec with Matchers:

  "Swap on an empty list" should "fail" in{
    val list = MemoryList(List(), List())
    list.swap(3, 4) match
      case Failure(_) =>
      case Success(_) => fail()
  }

  "Swap(0,0) on an empty list" should "fail" in {
    val list = MemoryList(List(), List())
    list.swap(0, 0) match
      case Failure(e) =>
      case Success(_) => fail()
  }

  "A correct swap" should "not fail" in {
    val list = MemoryList(List(0,1,5), List())
    list.swap(0, 2) match
      case Failure(_) => fail()
      case Success(l) => assert(l == MemoryList(List(5,1,0), List(Step.Swap(0,2))))
  }



