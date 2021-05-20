package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256Digest

case class DLCRefundSigsDb(
    dlcId: Sha256Digest,
    accepterSig: PartialSignature,
    initiatorSig: Option[PartialSignature])
