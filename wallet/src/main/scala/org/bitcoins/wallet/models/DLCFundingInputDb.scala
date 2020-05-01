package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.Sha256DigestBE

case class DLCFundingInputDb(
    eventId: Sha256DigestBE,
    isInitiator: Boolean,
    outPoint: TransactionOutPoint,
    output: TransactionOutput,
    sigs: Vector[PartialSignature]) {

  def toOutputReference: OutputReference =
    OutputReference(outPoint, output)
}
