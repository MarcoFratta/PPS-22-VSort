package view

import model.component.HasName

enum InputType:
  case Slider(min: Int, max: Int, name: String, defaultValue: Int)
  case Text(text: String)
  case SelectList[T <: HasName](name: String, list: List[T])