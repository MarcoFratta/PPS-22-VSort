package controller

import model.InputType

trait Controller:
  def getInputList: List[InputType]

class ControllerImpl() extends Controller:
  override def getInputList: List[InputType] =
    List(InputType.SelectList(List("Bubble", "Merge")), InputType.SelectList(List("Normal", "random")),
      InputType.Text("Max"), InputType.Text("Min"), InputType.Slider(10, 100, "size", 50, v => StepController.changeSize(v)))


