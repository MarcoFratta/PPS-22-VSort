package model

enum Step:
    case Swap(a: Int, b: Int)
    case Selection(a: Int)
    case Deselection(a: Int)
    case Comparison(a: Int, b: Int)

