package org.bitcoins.core.protocol.tlv

sealed trait DLCOutcomeType {
  def stringForJson: String // TODO get rid of this by turning Json maps in DLCMessage.scala into arrays
}

object DLCOutcomeType {

  def fromJsonString(str: String): DLCOutcomeType = {
    if (str.startsWith("ENUM:")) {
      EnumOutcome(str.drop("ENUM:".length))
    } else {
      throw new IllegalArgumentException(s"Unknown prefix: $str")
    }
  }
}

case class EnumOutcome(outcome: String) extends DLCOutcomeType {
  override def stringForJson: String = s"ENUM:$outcome"
}
