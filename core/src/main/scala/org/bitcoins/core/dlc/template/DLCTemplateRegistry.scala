package org.bitcoins.core.dlc.template

trait DLCTemplateBuilder {
  def `type`: String
  def create(): DLCTemplate
}

object DLCTemplateRegistry {

  @volatile
  private var registry: Map[String, DLCTemplateBuilder] = Map()

  register(ContractForDifferenceTemplateBuilder)

  def register(builder: DLCTemplateBuilder): Unit = synchronized {
    registry = registry.updated(builder.`type`, builder)
  }

  def newDLCTemplate(templateType: String): DLCTemplate = synchronized {
    registry.get(templateType) match {
      case Some(builder) => builder.create()
      case None =>
        throw new IllegalArgumentException(
          s"Unknown contract template type: `$templateType`")
    }
  }

}
