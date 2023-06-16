package model

enum Step:
    case Swap(a: Int, b: Int)
    case Selection[K](s: K, a: Int)
    case Deselection[K](s: K)
    case Comparison(a: Int, b: Int)

