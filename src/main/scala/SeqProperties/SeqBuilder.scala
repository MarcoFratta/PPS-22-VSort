// create enum/ class for type of distriubution
package SeqProperties

case class SeqBuilder(
    min: Int = 0,
    max: Int = 100,
    size: Int = 20,
    distribution: String = "random"){
  def min(minVal: Int): SeqBuilder = copy(min = minVal)
  def max(maxVal: Int): SeqBuilder = copy(max = maxVal)
  def size(size: Int): SeqBuilder = copy(size = size)
  def build(): Seq[Int] = SeqGenerator(min, max, size, "random").createSet()
}