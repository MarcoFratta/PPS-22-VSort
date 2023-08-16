package model.api

import model.api.Loopable.*
import model.api.SortAddOns
import model.api.Sortable.*
import model.api.SortableOps.*

import scala.language.postfixOps

object SortOperations:

  export model.api.Comparable
  export model.api.SortAddOns.*
  export model.api.SortableOps.*
  export model.api.LoopOperation.*
  export model.api.Loopable
  export model.api.Sortable
  export Step.*


  given comp: Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  given c1[T]: Conversion[T, Monad[T]] with
    def apply(x: T): Monad[T] = x!




