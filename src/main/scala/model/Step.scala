package model

enum Step:
    case Swap[K](a: K, b: K)
    case Selection[K, I](s: K, a: I)
    case Deselection[K](s: K)
    case Comparison[K](a: K, b: K)
    case Divide[K](start: K, stop: K)



