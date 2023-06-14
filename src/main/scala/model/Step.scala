package model

enum Step:
    case Swap(a: Int, b: Int)
    case Selection(s: String, a: Int)
    case Deselection(s: String)
    case Comparison(a: Int, b: Int)

