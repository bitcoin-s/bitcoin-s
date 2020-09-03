package org.bitcoins.dlc.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256Digest}

case class DLCDb(
    eventId: Sha256Digest,
    state: DLCState,
    isInitiator: Boolean,
    account: HDAccount,
    keyIndex: Int,
    oracleSigOpt: Option[SchnorrDigitalSignature]) {

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState)
  }
}
