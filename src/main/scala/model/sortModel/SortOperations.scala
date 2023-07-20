package model.sortModel

import model.Step
import model.sortModel.Loopable.*
import model.sortModel.SortAddOns
import model.sortModel.Sortable.*
import model.sortModel.SortableOps.*

import scala.language.postfixOps

object SortOperations:

  export model.sortModel.Comparable
  export model.sortModel.SortAddOns.*
  export model.sortModel.SortableOps.*
  export model.sortModel.LoopOperation.*
  export model.sortModel.Loopable
  export model.sortModel.Sortable
  export model.Step.*


  given comp: Comparable[Int] with
    override def compare(a: Int, b: Int): Boolean = a - b > 0

  given c1[T]: Conversion[T, Monad[T]] with
    def apply(x: T): Monad[T] = x!




