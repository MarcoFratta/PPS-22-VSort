package controller.component

import controller.*
import model.*
import model.component.*
import view.*

trait Properties extends ModelTypes:
  def params: Map[Params, ParamsType]
  def algorithm: Algorithm[ValType, ResultType] with HasName
  def distribution: Distribution[ParamsType, ValType] with HasName

object Properties:

  def defaultProperty(c: Algorithms with Distributions with IntTypes): Properties with IntTypes =
    Properties(c.algorithms.toList.minBy(_.name),
      c.distributions.toList.minBy(_.name),
      c.distributions.toList.minBy(_.name).params.map(a => a -> 10).toMap)

  def apply(algorithm: Algorithm[Int, Seq[State[Int]]] with HasName,
            distribution: Distribution[Int, Int] with HasName,
            params: Map[Params, Int]): Properties with IntTypes =
    PropertiesImpl(algorithm, distribution, params)

  private case class PropertiesImpl(override val algorithm: Algorithm[Int, Seq[State[Int]]] with HasName,
                                    override val distribution: Distribution[Int, Int] with HasName,
                                    override val params: Map[Params, Int]) extends Properties with IntTypes
