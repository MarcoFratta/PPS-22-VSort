package model

enum InputType:
  case Slider(min: Int, max: Int, name: String, defaultValue: Int)
  case Text(text: String)
  case SelectList[T](list: List[T])