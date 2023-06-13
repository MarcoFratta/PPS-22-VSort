// create enum/ class for type of distriubution
package SetProperties

case class SetBuilder(
    min: Int = 0,
    max: Int = 100,
    size: Int = 20,
    distribution: String = "random"){
  def min(minVal: Int): SetBuilder = copy(min = minVal)
  def max(maxVal: Int): SetBuilder = copy(max = maxVal)
  def size(size: Int): SetBuilder = copy(size = size)

  def build() = Generator(min, max, size, "random").createSet()
}