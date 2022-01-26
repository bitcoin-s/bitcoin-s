package org.bitcoins.core.api.dlc.wallet.db

import org.bitcoins.core.api.db.LastUpdatedDb
import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.protocol.tlv.DLCSerializationVersion
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  SchnorrDigitalSignature,
  Sha256Digest
}
import scodec.bits.ByteVector

import java.time.Instant

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
    lastUpdated: Instant,
    fundingOutPointOpt: Option[TransactionOutPoint],
    fundingTxIdOpt: Option[DoubleSha256DigestBE],
    closingTxIdOpt: Option[DoubleSha256DigestBE],
    aggregateSignatureOpt: Option[SchnorrDigitalSignature],
    serializationVersion: DLCSerializationVersion
) extends LastUpdatedDb {

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState, lastUpdated = TimeUtil.now)
  }

  def updateFundingOutPoint(outPoint: TransactionOutPoint): DLCDb = {
    copy(fundingOutPointOpt = Some(outPoint),
         fundingTxIdOpt = Some(outPoint.txIdBE),
         lastUpdated = TimeUtil.now)
  }

  def updateClosingTxId(txId: DoubleSha256DigestBE): DLCDb = {
    copy(closingTxIdOpt = Some(txId), lastUpdated = TimeUtil.now)
  }

  def updateContractId(id: ByteVector): DLCDb = {
    copy(contractIdOpt = Some(id), lastUpdated = TimeUtil.now)
  }

  def updateAggregateSignature(sig: SchnorrDigitalSignature): DLCDb = {
    copy(aggregateSignatureOpt = Some(sig), lastUpdated = TimeUtil.now)
  }
}
