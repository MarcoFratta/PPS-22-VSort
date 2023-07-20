package model

enum InputType:
  case Slider(min: Int, max: Int, name: String, defaultValue: Int, f: Int => Unit )
  case Text(text: String)
  case SelectList[T](list: List[T])