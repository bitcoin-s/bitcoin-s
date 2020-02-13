package org.bitcoins.core.wallet.utxo

import org.bitcoins.testkit.util.BitcoinSUnitTest

class TxoStateTest extends BitcoinSUnitTest {

  behavior of "TxoState"

  it must "read from string" in {
    TxoState.fromString("doesnotexist").get must be(TxoState.DoesNotExist)

    TxoState.fromString("PendingConfirmationsReceived").get must be(
      TxoState.PendingConfirmationsReceived)

    TxoState.fromString("ConfirmedReceived").get must be(
      TxoState.ConfirmedReceived)

    TxoState.fromString("Reserved").get must be(TxoState.Reserved)

    TxoState.fromString("PendingConfirmationsSpent").get must be(
      TxoState.PendingConfirmationsSpent)

    TxoState.fromString("ConfirmedSpent").get must be(TxoState.ConfirmedSpent)
  }
}
