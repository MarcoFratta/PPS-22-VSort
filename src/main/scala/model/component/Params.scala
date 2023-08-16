package model.component

import model.component.Params
import model.properties.HasRange

enum Params(minV: Int, maxV:Int) extends HasRange(minV, maxV):
  case Max extends Params(0, 100)
  case Min extends Params(0, 100)
  case Std extends Params(20, 100)
  case Size extends Params(0,100)
  case DuplicatesPercentage extends Params(0,100)

  override def min: Int = super.min

  override def max: Int = super.max

