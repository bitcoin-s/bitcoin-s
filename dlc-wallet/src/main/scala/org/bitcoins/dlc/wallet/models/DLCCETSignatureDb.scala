package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, Sha256DigestBE}

case class DLCCETSignatureDb(
    paramHash: Sha256DigestBE,
    isInitiator: Boolean,
    outcome: String,
    signature: ECAdaptorSignature) {
  def toTuple: (String, ECAdaptorSignature) = (outcome, signature)
}
