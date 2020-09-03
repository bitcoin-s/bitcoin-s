package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256Digest

case class DLCRefundSigDb(
    eventId: Sha256Digest,
    isInitiator: Boolean,
    refundSig: PartialSignature)
