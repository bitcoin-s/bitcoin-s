package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, ECPublicKey, Sha256Digest}

case class DLCCETSignaturesDb(
    dlcId: Sha256Digest,
    index: Long,
    sigPoint: ECPublicKey,
    accepterSig: ECAdaptorSignature,
    initiatorSig: Option[ECAdaptorSignature]
)
