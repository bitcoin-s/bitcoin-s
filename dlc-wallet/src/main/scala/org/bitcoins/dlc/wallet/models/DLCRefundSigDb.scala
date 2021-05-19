package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256Digest

case class DLCRefundSigDb(
    dlcId: Sha256Digest,
    acceptSig: PartialSignature,
    initiatorSig: Option[PartialSignature])
