package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, Sha256Digest, Sha256DigestBE}

case class DLCCETSignatureDb(
    paramHash: Sha256DigestBE,
    outcomeHash: Sha256Digest,
    signature: ECAdaptorSignature) {
  def toTuple: (Sha256Digest, ECAdaptorSignature) = (outcomeHash, signature)
}
