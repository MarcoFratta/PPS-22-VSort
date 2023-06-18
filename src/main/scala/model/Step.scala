package model

enum Step:
    case Swap[K](a: K, b: K)
    case Selection[K](s: K, a: K)
    case Deselection[K](s: K)
    case Comparison[K](a: K, b: K)

object Step:

    extension (steps: Seq[Step])
        def +(step: Step): Seq[Step] = steps :+ step

