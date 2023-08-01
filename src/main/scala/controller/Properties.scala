package controller

import controller.*
import controller.StepController.SeqProperties
import model.*
import view.*

trait Properties extends ModelTypes:
  def params: Map[Params, ParamsType]
  def algorithm: Algorithm[ValType, ResultType] with HasName
  def distribution: Distribution[ParamsType, ValType] with HasName

object Properties:
  private case class PropertiesImpl(override val algorithm: Algorithm[Int,Seq[State[Int]]] with HasName,
                                           override val distribution: Distribution[Int,Int] with HasName,
                                           override val params: Map[Params, Int]) extends Properties with IntTypes
  def apply(algorithm: Algorithm[Int, Seq[State[Int]]] with HasName,
                   distribution: Distribution[Int, Int] with HasName,
                   params: Map[Params, Int]): Properties with IntTypes =
    PropertiesImpl(algorithm, distribution, params)
