package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, ECPublicKey, Sha256DigestBE}

case class DLCCETSignatureDb(
    paramHash: Sha256DigestBE,
    isInitiator: Boolean,
    sigPoint: ECPublicKey,
    signature: ECAdaptorSignature) {
  def toTuple: (ECPublicKey, ECAdaptorSignature) = (sigPoint, signature)
}
