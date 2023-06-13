package model.SeqProperties

object Distribution:
  extension [T](s:scala.Seq[T])
    def ordered(using o:Ordering[T]):scala.Seq[T] = s.sorted
