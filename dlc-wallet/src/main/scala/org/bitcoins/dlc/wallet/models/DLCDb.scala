package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.crypto._
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
