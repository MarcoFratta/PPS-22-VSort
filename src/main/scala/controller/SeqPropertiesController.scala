package controller

trait SeqPropertiesController:
  def getAlgorithmList: List[String]
  def getDistributionList: List[String]


class SeqPropertiesControllerImpl extends SeqPropertiesController:
  override def getAlgorithmList: List[String] = List("bubble", "heap")
  override def getDistributionList: List[String] = List("Gaussian", "Random")
  
    