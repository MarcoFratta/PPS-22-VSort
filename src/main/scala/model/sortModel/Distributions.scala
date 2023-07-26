package model.sortModel

import model.seqProperties.Modifier.Duplicated
import model.seqProperties.*
import model.*

object Distributions:
  case class GaussianDistribution[T: Generable](mean: Int, std: Int, mi: Int, ma: Int, percentage: Int)
    extends GaussianGen[T](mean, std)
      with Shifted[T](mi, ma)
      with Duplicated(percentage / 100)


  case class UniformDistribution[T: Generable](mi:Int,ma:Int, percentage: Int)
    extends UniformGen[T](0, Int.MaxValue)
      with Shifted[T](mi, ma)
      with Duplicated(percentage / 100)





