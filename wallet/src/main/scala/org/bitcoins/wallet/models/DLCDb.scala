package org.bitcoins.wallet.models

import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}

case class DLCDb(
    eventId: Sha256DigestBE,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    refundSigOpt: Option[PartialSignature],
    oracleSigOpt: Option[SchnorrDigitalSignature])
