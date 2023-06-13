package SetProperties


trait Generator:
  def createSet(): Seq[Int]

object Generator:
  def apply(min: Int, max: Int, size: Int, distribution: String): Generator =
    GeneratorImpl(min: Int, max: Int, size: Int, distribution: String)
  private case class GeneratorImpl(min: Int, max: Int, size: Int, distribution: String) extends Generator:
    val rndm = new scala.util.Random()
    override def createSet(): Seq[Int] =
      Array.fill(size)(rndm.between(min, max)).toSeq
