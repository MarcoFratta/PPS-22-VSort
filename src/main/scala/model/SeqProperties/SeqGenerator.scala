package model.SeqProperties

trait SeqGenerator:
  def createSet(): Seq[Int]

object SeqGenerator:
  def apply(min: Int, max: Int, size: Int, distribution: String): SeqGenerator =
    SeqGeneratorImpl(min: Int, max: Int, size: Int, distribution: String)
  private case class SeqGeneratorImpl(min: Int, max: Int, size: Int, distribution: String) extends SeqGenerator:
    private val rndm = new scala.util.Random()
    override def createSet(): Seq[Int] = 
      if min > max || size <= 0 then throw new IllegalArgumentException()
      Array.fill(size)(rndm.between(min, max)).toSeq
