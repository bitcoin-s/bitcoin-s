package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256DigestBE

case class DLCRefundSigDb(
    paramHash: Sha256DigestBE,
    isInitiator: Boolean,
    refundSig: PartialSignature)
