package org.bitcoins.dlc.oracle

import org.bitcoins.crypto.CryptoTrait
import scodec.bits.ByteVector

/** Represents a single DLC event that the oracle is going to sign */
sealed trait DLCAttestationType extends CryptoTrait {
  def bytes: ByteVector
  def outcomeString: String
}

case class EnumAttestation(outcomeString: String) extends DLCAttestationType {
  def bytes: ByteVector = cryptoRuntime.serializeForHash(outcomeString)
}

case class RangeAttestation(outcome: Long) extends DLCAttestationType {
  override def outcomeString: String = outcome.toString
  def bytes: ByteVector = cryptoRuntime.serializeForHash(outcomeString)
}

case class DigitDecompositionSignAttestation(positive: Boolean)
    extends DLCAttestationType {
  override def outcomeString: String = if (positive) "+" else "-"
  def bytes: ByteVector = cryptoRuntime.serializeForHash(outcomeString)
}

case class DigitDecompositionAttestation(outcome: Int)
    extends DLCAttestationType {
  override def outcomeString: String = outcome.toString
  def bytes: ByteVector = cryptoRuntime.serializeForHash(outcomeString)
}
