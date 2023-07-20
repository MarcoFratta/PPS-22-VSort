package controller

import model.InputType

class MainController:
  def getInputList: List[InputType] =
    List(InputType.SelectList(List("Bubble", "Merge")), InputType.SelectList(List("Normal", "random")),
      InputType.Text("Max"), InputType.Text("Min"), InputType.Slider(10, 100, "size", 50, v => StepController.changeSize(v)))


