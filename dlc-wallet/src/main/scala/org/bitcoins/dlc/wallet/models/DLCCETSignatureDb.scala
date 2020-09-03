package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, Sha256Digest}

case class DLCCETSignatureDb(
    eventId: Sha256Digest,
    outcomeHash: Sha256Digest,
    signature: ECAdaptorSignature) {
  def toTuple: (Sha256Digest, ECAdaptorSignature) = (outcomeHash, signature)
}
