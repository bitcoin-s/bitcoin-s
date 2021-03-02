package org.bitcoins.core.wallet.utxo

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class TxoStateTest extends BitcoinSUnitTest {

  behavior of "TxoState"

  it must "read from string" in {
    TxoState.fromString("doesnotexist") must be(TxoState.DoesNotExist)

    TxoState.fromString("PendingConfirmationsReceived") must be(
      TxoState.PendingConfirmationsReceived)

    TxoState.fromString("ConfirmedReceived") must be(TxoState.ConfirmedReceived)

    TxoState.fromString("Reserved") must be(TxoState.Reserved)

    TxoState.fromString("PendingConfirmationsSpent") must be(
      TxoState.PendingConfirmationsSpent)

    TxoState.fromString("ConfirmedSpent") must be(TxoState.ConfirmedSpent)
  }
}
