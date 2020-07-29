package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}

case class DLCDb(
    eventId: Sha256DigestBE,
    state: DLCState,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    oracleSigOpt: Option[SchnorrDigitalSignature]) {

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState)
  }
}
