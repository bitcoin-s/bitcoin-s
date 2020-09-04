package org.bitcoins.dlc.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import scodec.bits.ByteVector

case class DLCDb(
    paramHash: Sha256DigestBE,
    tempContractId: Sha256DigestBE,
    contractIdOpt: Option[ByteVector],
    state: DLCState,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    oracleSigOpt: Option[SchnorrDigitalSignature]) {

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState)
  }
}
