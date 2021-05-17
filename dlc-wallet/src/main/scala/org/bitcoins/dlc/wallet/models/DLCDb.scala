package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.dlc.models.{DLCState, SingleOracleInfo}
import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  SchnorrDigitalSignature,
  Sha256Digest,
  Sha256DigestBE
}
import scodec.bits.ByteVector

case class DLCDb(
    paramHash: Sha256DigestBE,
    tempContractId: Sha256Digest,
    contractIdOpt: Option[ByteVector],
    state: DLCState,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    oracleSigsOpt: Option[Vector[SchnorrDigitalSignature]],
    fundingOutPointOpt: Option[TransactionOutPoint],
    fundingTxIdOpt: Option[DoubleSha256DigestBE],
    closingTxIdOpt: Option[DoubleSha256DigestBE],
    outcomesOpt: Option[Vector[DLCOutcomeType]],
    oraclesUsedOpt: Option[Vector[SingleOracleInfo]]
) {

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState)
  }

  def updateFundingOutPoint(outPoint: TransactionOutPoint): DLCDb = {
    copy(fundingOutPointOpt = Some(outPoint),
         fundingTxIdOpt = Some(outPoint.txIdBE))
  }
}
