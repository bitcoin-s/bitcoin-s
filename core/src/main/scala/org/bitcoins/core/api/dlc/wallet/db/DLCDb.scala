package org.bitcoins.core.api.dlc.wallet.db

import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  SchnorrDigitalSignature,
  Sha256Digest
}
import scodec.bits.ByteVector

/** This table contains all the meta information about a DLC.
  * This includes various identifiers as well as state and a BIP 32 key path.
  */
case class DLCDb(
    dlcId: Sha256Digest,
    tempContractId: Sha256Digest,
    contractIdOpt: Option[ByteVector],
    protocolVersion: Int,
    state: DLCState,
    isInitiator: Boolean,
    account: HDAccount,
    changeIndex: HDChainType,
    keyIndex: Int,
    feeRate: SatoshisPerVirtualByte,
    fundOutputSerialId: UInt64,
    fundingOutPointOpt: Option[TransactionOutPoint],
    fundingTxIdOpt: Option[DoubleSha256DigestBE],
    closingTxIdOpt: Option[DoubleSha256DigestBE],
    aggregateSignatureOpt: Option[SchnorrDigitalSignature]
) {

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState)
  }

  def updateFundingOutPoint(outPoint: TransactionOutPoint): DLCDb = {
    copy(fundingOutPointOpt = Some(outPoint),
         fundingTxIdOpt = Some(outPoint.txIdBE))
  }
}
