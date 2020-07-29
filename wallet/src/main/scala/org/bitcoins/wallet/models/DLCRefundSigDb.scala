package org.bitcoins.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256DigestBE

case class DLCRefundSigDb(
    eventId: Sha256DigestBE,
    isInitiator: Boolean,
    refundSig: PartialSignature)
