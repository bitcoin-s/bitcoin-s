package org.bitcoins.dlc.wallet.models

import org.bitcoins.crypto.{ECAdaptorSignature, ECPublicKey, Sha256Digest}

case class DLCCETSignatureDb(
    dlcId: Sha256Digest,
    index: Long,
    isInitiator: Boolean,
    sigPoint: ECPublicKey,
    adaptorSig: ECAdaptorSignature
)
