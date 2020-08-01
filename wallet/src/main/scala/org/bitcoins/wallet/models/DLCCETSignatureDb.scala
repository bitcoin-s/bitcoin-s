package org.bitcoins.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, Sha256DigestBE}

case class DLCCETSignatureDb(
    eventId: Sha256DigestBE,
    outcomeHash: Sha256DigestBE,
    signature: ECAdaptorSignature) {
  def toTuple: (Sha256DigestBE, ECAdaptorSignature) = (outcomeHash, signature)
}
