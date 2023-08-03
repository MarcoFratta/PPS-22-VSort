package model

import model.seqProperties.HasRange

enum Params(minV: Int, maxV:Int) extends HasRange(minV, maxV):
  case Max extends Params(0, 100)
  case Min extends Params(0, 100)
  case Std extends Params(0, 100)
  case Size extends Params(0,150)
  case DuplicatesPercentage extends Params(0,100)

  override def min: Int = super.min

  override def max: Int = super.max

