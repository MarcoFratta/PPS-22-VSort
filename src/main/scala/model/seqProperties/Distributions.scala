package model.seqProperties

import model.*
import model.seqProperties.*
import model.seqProperties.Modifier.Duplicated

object Distributions:
  case class GaussianDistribution[T: Generable](mean: Int, std: Int, mi: Int, ma: Int, percentage: Int)
    extends GaussianGen[T](mean, std)
      with Shifted[T](mi, ma)
      with Duplicated(percentage.toDouble / 100)


  case class UniformDistribution[T: Generable](mi:Int,ma:Int, percentage: Int)
    extends UniformGen[T](mi, ma)
      with Duplicated(percentage.toDouble / 100)





