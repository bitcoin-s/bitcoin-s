package org.bitcoins.core.wallet.utxo

import org.bitcoins.testkit.util.BitcoinSUnitTest

class TxoStateTest extends BitcoinSUnitTest {

  behavior of "TxoState"

  it must "read from string" in {
    TxoState.fromString("doesnotexist").get must be (TxoState.DoesNotExist)

    TxoState.fromString("PendingReceived").get must be (TxoState.UnconfirmedReceived)

    TxoState.fromString("ConfirmedReceived").get must be (TxoState.ConfirmedReceived)

    TxoState.fromString("PendingSpent").get must be (TxoState.UnconfirmedSpent)

    TxoState.fromString("ConfirmedSpent").get must be (TxoState.ConfirmedSpent)
  }
}
