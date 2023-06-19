package model

object SortingAlgorithms {

  // merge sort funzionale ma senza step
  def mergeSort(seq: List[Int]): List[Int] = seq match
    case Nil => Nil
    case xs :: Nil => List(xs)
    case _ =>
      val (left, right) = seq splitAt seq.length / 2
      merge(mergeSort(left), mergeSort(right))

  private def merge(seq1: List[Int], seq2: List[Int]): List[Int] =
    (seq1, seq2) match {
      case (Nil, _) => seq2
      case (_, Nil) => seq1
      case (x :: xs, y :: ys) =>
        if (x < y) x :: merge(xs, seq2)
        else y :: merge(seq1, ys)
    }

}
