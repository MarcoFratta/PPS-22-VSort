package model.seqProperties

import model.*
import model.seqProperties.*
import model.seqProperties.Modifier.Duplicated
import model.sortModel.Comparable

import scala.util.Random

object Distributions:
  case class GaussianDistribution[T: Generable](mean: Int, std: Int, mi: Int, ma: Int)
    extends GaussianGen[T](mean, std)
      with Shifted[T](mi, ma)


  case class UniformDistribution[T: Generable](mi: Int, ma: Int, percentage: Int)
    extends UniformGen[T](mi, ma)
      with Duplicated(percentage.toDouble / 100)

  def intParams(p: Map[Params, Int]): Conversion[Params, Int] =
    (x: Params) => p(x)





