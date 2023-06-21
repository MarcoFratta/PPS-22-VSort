package model

object SortingAlgorithms {

  def mergesort(seq: List[Int]): List[Int] = seq.length match
    case n if n < 2 => seq
    case _ =>
      val l = seq.splitAt(seq.length/2)._1
      val r = seq.splitAt(seq.length/2)._2
      merge(mergesort(l), mergesort(r))

  private def merge(l: List[Int], r: List[Int]): List[Int] =
    var list = l.concat(r)

    var i = 0
    var j = l.length

    while (j < list.length) {
      if list(i) < list(j) then
        i = i + 1
      else
        for (k <- j until i by -1) {
          list = list.updated(k, list(k-1)).updated(k-1, list(k))
        }
        i = i + 1
        j = j + 1
    }

    list

}
