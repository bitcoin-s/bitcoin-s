package org.bitcoins.core.api.dlcoracle

import org.bitcoins.crypto.CryptoUtil
import scodec.bits.ByteVector

/** Represents a single DLC event that the oracle is going to sign */
sealed trait DLCAttestationType {
  def bytes: ByteVector
  def outcomeString: String
}

case class EnumAttestation(outcomeString: String) extends DLCAttestationType {
  def bytes: ByteVector = CryptoUtil.serializeForHash(outcomeString)
}

sealed trait DigitDecompositionAttestationType extends DLCAttestationType

case class DigitDecompositionSignAttestation(positive: Boolean)
    extends DigitDecompositionAttestationType {
  override def outcomeString: String = if (positive) "+" else "-"
  def bytes: ByteVector = CryptoUtil.serializeForHash(outcomeString)
}

case class DigitDecompositionAttestation(outcome: Int)
    extends DigitDecompositionAttestationType {
  override def outcomeString: String = outcome.toString
  def bytes: ByteVector = CryptoUtil.serializeForHash(outcomeString)
}
