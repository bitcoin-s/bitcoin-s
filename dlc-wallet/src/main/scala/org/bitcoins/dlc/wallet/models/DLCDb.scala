package org.bitcoins.dlc.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  SchnorrDigitalSignature,
  Sha256DigestBE
}

case class DLCDb(
    eventId: Sha256DigestBE,
    state: DLCState,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    oracleSigOpt: Option[SchnorrDigitalSignature],
    fundingTxIdOpt: Option[DoubleSha256DigestBE],
    closingTxIdOpt: Option[DoubleSha256DigestBE]) {

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState)
  }

  def setFundingTxId(txId: DoubleSha256DigestBE): DLCDb = {
    copy(fundingTxIdOpt = Some(txId))
  }

  def setClosingTxId(txId: DoubleSha256DigestBE): DLCDb = {
    copy(closingTxIdOpt = Some(txId))
  }
}
