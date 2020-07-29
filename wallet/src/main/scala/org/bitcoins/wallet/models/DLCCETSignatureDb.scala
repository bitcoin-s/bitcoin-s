package org.bitcoins.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256DigestBE

case class DLCCETSignatureDb(
    eventId: Sha256DigestBE,
    outcomeHash: Sha256DigestBE,
    signature: PartialSignature) {
  def toTuple: (Sha256DigestBE, PartialSignature) = (outcomeHash, signature)
}
