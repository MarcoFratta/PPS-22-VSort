package model

import model.sortModel.Comparable

object IntOrderings:
  val ascendingXOrder: Comparable[(Int, Int)] = (a, b) => a._1 - b._1 > 0
  val descendingXOrder: Comparable[(Int, Int)] = (a, b) => b._1 - a._1 > 0
  val randomOrder: Comparable[(Int, Int)] = (a, b) => util.Random.nextBoolean()
  val ascendingYOrder: Comparable[(Int, Int)] = (a, b) => b._2 - a._2 > 0
  val descendingYOrder: Comparable[(Int, Int)] = (a, b) => a._2 - b._2 > 0