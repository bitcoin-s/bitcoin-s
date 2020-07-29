package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256DigestBE}

case class DLCCETSignatureDb(
    paramHash: Sha256DigestBE,
    isInitiator: Boolean,
    outcome: DLCOutcomeType,
    signature: ECAdaptorSignature) {
  def toTuple: (DLCOutcomeType, ECAdaptorSignature) = (outcome, signature)
}
